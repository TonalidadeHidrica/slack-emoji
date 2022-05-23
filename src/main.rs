use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io::{BufRead, BufReader, Read, Write},
    path::PathBuf,
    str::FromStr,
    sync::mpsc,
    thread::sleep,
    time::Duration,
};

use anyhow::{bail, Context};
use chrono::{DateTime, Local, Utc};
use clap::Parser;
use derive_more::{Display, FromStr};
use fs_err::{File, OpenOptions};
use itertools::Itertools;
use log::{error, info};
use once_cell::unsync::Lazy;
use reqwest::blocking::{multipart, Client};
use serde::{
    de::{self, DeserializeOwned, Unexpected},
    Deserialize, Deserializer,
};
use serde_with::{rust::string_empty_as_none, serde_as, TimestampSeconds};
use url::Url;

#[derive(Deserialize)]
struct Config {
    tokens: HashMap<WorkspaceDomain, TokenConfig>,
}
#[derive(Deserialize)]
struct TokenConfig {
    xoxc: String,
    xoxd: String,
}

#[derive(clap::Parser)]
struct Args {
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(clap::Subcommand)]
enum Subcommand {
    MakeSpreadsheet(MakeSpreadsheet),
    #[clap(alias("cp"))]
    CopyEmojis(CopyEmojis),
    MakeQueries(MakeQueries),
}

#[derive(clap::Args)]
struct MakeSpreadsheet {}

#[derive(clap::Args)]
/// alias: cp
struct CopyEmojis {
    file: PathBuf,
}

#[derive(clap::Args)]
struct MakeQueries {
    emoji_list: PathBuf,
    src: WorkspaceDomain,
    dst: WorkspaceDomain,
}

#[derive(Debug)]
struct CopyQuery {
    src: EmojiLocation,
    dst: WorkspaceEmoji,
}
#[derive(Debug)]
enum EmojiLocation {
    Path(PathBuf),
    Workspace(WorkspaceEmoji),
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct WorkspaceEmoji {
    workspace: WorkspaceDomain,
    name: EmojiName,
}
#[derive(Debug)]
struct EmojiDestination {
    workspace: WorkspaceDomain,
    name: Option<EmojiName>,
}

impl FromStr for EmojiLocation {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains(':') {
            Ok(EmojiLocation::Workspace(s.parse()?))
        } else {
            Ok(EmojiLocation::Path(s.into()))
        }
    }
}
impl FromStr for WorkspaceEmoji {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s.splitn(2, ':').collect_vec()[..] {
            [x, y] => Ok(WorkspaceEmoji {
                workspace: WorkspaceDomain(x.to_string()),
                name: EmojiName(y.to_string()),
            }),
            _ => bail!("Workspace emoji specifier should contain ':', found {s:?}"),
        }
    }
}
impl FromStr for EmojiDestination {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains(':') {
            let WorkspaceEmoji { workspace, name } = s.parse()?;
            Ok(EmojiDestination {
                workspace,
                name: Some(name),
            })
        } else {
            Ok(EmojiDestination {
                workspace: WorkspaceDomain(s.to_owned()),
                name: None,
            })
        }
    }
}

impl Display for EmojiLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmojiLocation::Path(p) => write!(f, "{}", p.display()),
            EmojiLocation::Workspace(w) => w.fmt(f),
        }
    }
}
impl Display for WorkspaceEmoji {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.workspace.0, self.name.0)
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Args::parse();
    let config: Config = toml::from_str(&fs_err::read_to_string("config.toml")?)?;

    match &args.subcommand {
        Subcommand::MakeSpreadsheet(_) => make_spreadsheet(&config),
        Subcommand::CopyEmojis(args) => copy_emojis(&config, args),
        Subcommand::MakeQueries(args) => make_queries(&config, args),
    }
}

fn make_spreadsheet(config: &Config) -> anyhow::Result<()> {
    let client = Client::new();
    for (workspace, tokens) in &config.tokens {
        for emoji in get_emojis(&client, workspace, tokens)? {
            if let (false, Some(url)) = (emoji.is_alias, &emoji.url) {
                let values = [
                    emoji.name.0.clone(),
                    format!(r#"=IMAGE("{}")"#, url),
                    emoji.synonyms.iter().join(", "),
                ];
                println!("{}", values.join("\t"));
            }
        }
    }

    Ok(())
}

fn make_queries(config: &Config, args: &MakeQueries) -> Result<(), anyhow::Error> {
    let client = Client::new();
    let emojis = get_emojis(&client, &args.src, &config.tokens[&args.src])?;
    let emojis = emojis.iter().map(|e| (&e.name, e)).collect::<HashMap<_, _>>();
    for name in BufReader::new(File::open(&args.emoji_list)?).lines() {
        let name = EmojiName(name?);
        println!("{}:{}\t{}", args.src, name, args.dst);
        for alias in &emojis[&name].synonyms {
            if alias != &name {
                println!("{}:{}\t{}:{}", args.dst, name, args.dst, alias);
            }
        }
    }
    Ok(())
}

fn copy_emojis(config: &Config, args: &CopyEmojis) -> anyhow::Result<()> {
    let client = Client::new();

    let queries = BufReader::new(File::open(&args.file)?)
        .lines()
        .zip(1..)
        .map(|(line, i)| {
            (|| match &line?.trim().split_whitespace().take(3).collect_vec()[..] {
                [_, _, _, ..] => bail!("Too many arguments"),
                [src, dst] => {
                    let src: EmojiLocation = src.parse()?;
                    let dst: EmojiDestination = dst.parse()?;
                    let name = match dst.name {
                        Some(name) => name,
                        None => match &src {
                            EmojiLocation::Path(path) => {
                                let stem = path
                                    .file_stem()
                                    .with_context(|| format!("No filename found for {path:?}"))?;
                                let name = stem.to_str().with_context(|| {
                                    format!("Filename cannot be converted to UTF-8: {stem:?}")
                                })?;
                                EmojiName(name.to_owned())
                            }
                            EmojiLocation::Workspace(emoji) => emoji.name.clone(),
                        },
                    };
                    let query = CopyQuery {
                        src,
                        dst: WorkspaceEmoji {
                            workspace: dst.workspace,
                            name,
                        },
                    };
                    Ok((i, query))
                }
                _ => bail!("Not enough arguments"),
            })()
            .map_err(|e| e.context(format!("At line {i} in {:?}", args.file)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let emojis = queries
        .iter()
        .flat_map(|(_, q)| {
            let src = match &q.src {
                EmojiLocation::Path(_) => None,
                EmojiLocation::Workspace(w) => Some(&w.workspace),
            };
            [src, Some(&q.dst.workspace)]
        })
        .flatten()
        .collect::<HashSet<_>>()
        .into_iter()
        .map(|workspace| {
            let tokens = config
                .tokens
                .get(workspace)
                .with_context(|| format!("No token for {workspace:?} was found"))?;
            anyhow::Ok((workspace, get_emojis(&client, workspace, tokens)?))
        })
        .collect::<Result<HashMap<_, _>, _>>()?;
    let emojis = emojis
        .iter()
        .map(|(&w, emojis)| {
            let emojis: HashMap<_, _> = emojis.iter().map(|e| (&e.name, e)).collect();
            (w, emojis)
        })
        .collect::<HashMap<_, _>>();

    // Simulate the process of adding emojis and detect errors
    {
        let mut emojis = emojis
            .iter()
            .map(|(&k, v)| {
                let map: HashMap<_, _> = v.iter().map(|(&k, v)| (k, v.url.is_some())).collect();
                (k, map)
            })
            .collect::<HashMap<_, _>>();
        let mut has_error = false;
        for (i, query) in &queries {
            match &query.src {
                EmojiLocation::Path(p) => {
                    if !p.metadata().map_or(false, |m| m.is_file()) {
                        has_error = true;
                        error!("Line {i}: File {p:?} is not an existing file")
                    }
                }
                EmojiLocation::Workspace(src) => {
                    if src == &query.dst {
                        has_error = true;
                        error!("Line {i}: The src and dst are the same: {src:?}");
                    }
                    if let Some(&url_is_valid) = emojis[&src.workspace].get(&&src.name) {
                        if !url_is_valid {
                            has_error = true;
                            error!(
                                "Line {i}: The emoji does not point at an appropriate URL: {src:?}"
                            );
                        }
                    } else {
                        has_error = true;
                        error!(
                            "Line {i}: {:?} does/will not contain {:?}",
                            src.workspace, src.name
                        );
                    }
                }
            }
            {
                let emoji = &query.dst;
                let emojis = emojis.get_mut(&emoji.workspace).unwrap();
                if emojis.contains_key(&emoji.name) {
                    has_error = true;
                    error!(
                        "Line {i}: {:?} does/will contain {:?}",
                        emoji.workspace, emoji.name
                    );
                }
                emojis.insert(&emoji.name, true);
            }
        }
        if has_error {
            bail!("Aborting due to the errors above");
        }
    }

    let mut log = Lazy::<_, _>::new(|| {
        let mut name = args
            .file
            .file_name()
            .expect("We could open this file so this shuold never happen")
            .to_owned();
        name.push(".log");
        match OpenOptions::new()
            .append(true)
            .create(true)
            .open(args.file.with_file_name(name))
        {
            Ok(f) => Some(f),
            Err(e) => {
                error!("Could not open {:?}: {:?}", args.file, e);
                None
            }
        }
    });

    let (ctrlc_tx, ctrlc_rx) = mpsc::channel();
    ctrlc::set_handler(move || {
        let _ = ctrlc_tx.send(());
    })?;
    println!("Starting to process the query.  Press Ctrl+C to abort.");

    let mut new_emojis = HashMap::new();
    let mut stop = false;
    for (i, query) in &queries {
        if ctrlc_rx.try_recv().is_ok() && !stop {
            error!("Aborting on Ctrl-C.");
            stop = true;
        }

        let tokens = &config.tokens[&query.dst.workspace];
        if let Err(e) = (|| match &query.src {
            _ if stop => bail!("Aborted on Ctrl-C"),
            EmojiLocation::Path(path) => {
                upload_emoji(&client, tokens, path, &query.dst, &mut new_emojis)
            }
            EmojiLocation::Workspace(src) => {
                if src.workspace == query.dst.workspace {
                    info!(
                        "Adding alias in {:?} for {:?} as {:?}",
                        src.workspace, src.name, query.dst.name
                    );
                    let params = EmojiAddParams::<std::io::Empty> {
                        name: query.dst.name.clone(),
                        content: EmojiAddContent::Alias(src.name.clone()),
                    };
                    emoji_add(&client, &query.dst.workspace, tokens, params)
                } else if let Some(emoji) = emojis[&src.workspace].get(&src.name) {
                    copy_emoji_by_url(
                        &client,
                        tokens,
                        emoji.url.as_ref().unwrap(),
                        src,
                        &query.dst,
                        &mut new_emojis,
                    )
                } else if let Some(old_src) = new_emojis.get(&src) {
                    info!("{src:?} is a new emoji.  Using {old_src:?} as the source.");
                    match old_src {
                        NewEmojiSource::Path(path) => {
                            upload_emoji(&client, tokens, path, &query.dst, &mut new_emojis)
                        }
                        NewEmojiSource::Url(url) => copy_emoji_by_url(
                            &client,
                            tokens,
                            url,
                            src,
                            &query.dst,
                            &mut new_emojis,
                        ),
                    }
                } else {
                    bail!("{src:?} is a new emoji, but it was probably accidentally skipped")
                }
            }
        })() {
            let e = e
                .to_string()
                .chars()
                .filter(|&c| c != '\t' && c != '\r' && c != '\n')
                .collect::<String>();
            if let Some(log) = &mut *log {
                if let Err(e) =
                    writeln!(log, "{}\t{}\t{}\t{e}", Local::now(), &query.src, query.dst)
                {
                    error!("Failed to write to log file: {:?}", e);
                }
            }
            if !stop {
                error!("Failed to execute line {i} ({query:?}): {e}");
            }
        }
        if !stop {
            sleep(Duration::from_secs_f64(0.5));
        }
    }
    Ok(())
}

#[derive(Debug)]
enum NewEmojiSource<'a> {
    Url(&'a Url),
    Path(&'a PathBuf),
}

fn upload_emoji<'a>(
    client: &Client,
    tokens: &TokenConfig,
    #[allow(clippy::ptr_arg)] path: &'a PathBuf,
    dst: &'a WorkspaceEmoji,
    new_emojis: &mut HashMap<&'a WorkspaceEmoji, NewEmojiSource<'a>>,
) -> anyhow::Result<EmojiAddResponse> {
    info!("Uploading emoji from {path:?} to {:?}", dst);
    let params = EmojiAddParams {
        name: dst.name.clone(),
        content: EmojiAddContent::New(File::open(path)?),
    };
    new_emojis.insert(dst, NewEmojiSource::Path(path));
    emoji_add(client, &dst.workspace, tokens, params)
}

fn copy_emoji_by_url<'a>(
    client: &Client,
    tokens: &TokenConfig,
    url: &'a Url,
    src: &'a WorkspaceEmoji,
    dst: &'a WorkspaceEmoji,
    new_emojis: &mut HashMap<&'a WorkspaceEmoji, NewEmojiSource<'a>>,
) -> anyhow::Result<EmojiAddResponse> {
    info!("Copying emoji from {src:?} to {dst:?}");
    let data = client.get(url.clone()).send()?;
    let params = EmojiAddParams {
        name: dst.name.clone(),
        content: EmojiAddContent::New(data),
    };
    new_emojis.insert(dst, NewEmojiSource::Url(url));
    emoji_add(client, &dst.workspace, tokens, params)
}

fn get_emojis(
    client: &Client,
    workspace: &WorkspaceDomain,
    tokens: &TokenConfig,
) -> anyhow::Result<Vec<Emoji>> {
    let params = EmojiAdminListParams { page: 1, count: 1 };
    let res = emoji_admin_list(client, workspace, tokens, params)?;
    let params = EmojiAdminListParams {
        page: 1,
        count: res.paging.total,
    };
    let res = emoji_admin_list(client, workspace, tokens, params)?;
    Ok(res.emoji)
}

fn send<T: DeserializeOwned>(
    client: &Client,
    workspace: &WorkspaceDomain,
    tokens: &TokenConfig,
    method: &'static str,
    form: multipart::Form,
) -> anyhow::Result<T> {
    let form = form.text("token", tokens.xoxc.clone());
    let url = format!("https://{}-2020.slack.com/api/{}", workspace, method);
    let response = client
        .post(url)
        .header("cookie", format!("d={}", tokens.xoxd))
        .multipart(form)
        .send()?;
    Ok(response.json()?)
}

struct EmojiAdminListParams {
    page: usize,
    count: usize,
}
fn emoji_admin_list(
    client: &Client,
    workspace_domain: &WorkspaceDomain,
    tokens: &TokenConfig,
    params: EmojiAdminListParams,
) -> anyhow::Result<EmojiAdminList> {
    let form = multipart::Form::new()
        .text("page", params.page.to_string())
        .text("count", params.count.to_string())
        .text("sort_by", "name")
        .text("sort_dir", "asc")
        .text("queries", "[]")
        .text("user_ids", "[]");
    send(client, workspace_domain, tokens, "emoji.adminList", form)
}

struct EmojiAddParams<T> {
    name: EmojiName,
    content: EmojiAddContent<T>,
}
enum EmojiAddContent<T> {
    Alias(EmojiName),
    New(T),
}
fn emoji_add<T: Read + Send + 'static>(
    client: &Client,
    workspace_domain: &WorkspaceDomain,
    tokens: &TokenConfig,
    params: EmojiAddParams<T>,
) -> anyhow::Result<EmojiAddResponse> {
    let form = match params.content {
        EmojiAddContent::Alias(alias_for) => multipart::Form::new()
            .text("mode", "alias")
            .text("name", params.name.0)
            .text("alias_for", alias_for.0),
        EmojiAddContent::New(image) => {
            let image = multipart::Part::reader(image).file_name("dummy");
            multipart::Form::new()
                .text("mode", "data")
                .text("name", params.name.0)
                .part("image", image)
        }
    };
    let response: EmojiAddResponse = send(client, workspace_domain, tokens, "emoji.add", form)?;
    if let Some(error) = response.error {
        bail!("API returned error: {error:?}");
    } else if !response.ok {
        bail!("API returned error");
    }
    Ok(response)
}
#[derive(Debug, Deserialize)]
pub struct EmojiAddResponse {
    pub ok: bool,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Paging {
    pub count: usize,
    pub page: usize,
    pub pages: usize,
    pub total: usize,
}

#[derive(Debug, Deserialize)]
pub struct EmojiAdminList {
    pub custom_emoji_total_count: usize,
    pub disabled_emoji: Vec<Emoji>,
    pub emoji: Vec<Emoji>,
    pub ok: bool,
    pub paging: Paging,
}

#[serde_as]
#[derive(Debug, Deserialize)]
pub struct Emoji {
    #[serde(with = "string_empty_as_none")]
    pub alias_for: Option<String>,
    pub avatar_hash: String,
    pub can_delete: bool,
    #[serde_as(as = "TimestampSeconds<i64>")]
    pub created: DateTime<Utc>,
    #[serde(deserialize_with = "bool_from_int")]
    pub is_alias: bool,
    pub is_bad: bool,
    pub name: EmojiName,
    pub synonyms: Vec<EmojiName>,
    pub team_id: String,
    #[serde(with = "string_empty_as_none")]
    pub url: Option<Url>,
    pub user_display_name: String,
    pub user_id: String,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Display, Deserialize)]
pub struct EmojiName(pub String);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, FromStr, Display, Deserialize)]
#[serde(transparent)]
pub struct WorkspaceDomain(pub String);

fn bool_from_int<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    match u8::deserialize(deserializer)? {
        0 => Ok(false),
        1 => Ok(true),
        other => Err(de::Error::invalid_value(
            Unexpected::Unsigned(other as u64),
            &"zero or one",
        )),
    }
}
