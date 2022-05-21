use std::collections::HashMap;

use chrono::{DateTime, Utc};
use clap::Parser;
use derive_more::Display;
use itertools::Itertools;
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
}

#[derive(clap::Args)]
struct MakeSpreadsheet {}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Args::parse();

    match &args.subcommand {
        Subcommand::MakeSpreadsheet(_) => make_spreadsheet(),
    }
}

fn make_spreadsheet() -> anyhow::Result<()> {
    let client = Client::new();
    let config: Config = toml::from_str(&fs_err::read_to_string("config.toml")?)?;
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

#[derive(Debug, Display, Deserialize)]
pub struct EmojiName(pub String);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Display, Deserialize)]
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
