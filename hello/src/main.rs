#![warn(clippy::all)]
use std::collections::{HashSet, HashMap};
use regex::Regex;
use lazy_static::lazy_static;
use serenity::{model::prelude::*, prelude::*, Client, all::EditMessage};
use once_cell::sync::Lazy;
use tokio::sync::Mutex;  
use tokio::time::{sleep, Duration};

lazy_static! {
    static ref SENT: Mutex<HashSet<MessageId>> = Mutex::new(HashSet::new());
    static ref anti_troll_measure: Mutex<HashMap<MessageId, String>> = Mutex::new(HashMap::new());
}


fn helper(mut s: &str) -> &str {
    while let Some(ch) = s.chars().last() {
        if ".,;:)]}>\"'".contains(ch) { s = &s[..s.len() - ch.len_utf8()] } else { break; }
    }
    s
}

#[derive(Clone, Copy)]
enum Site { Twitter, Instagram, Reddit, TikTok }

fn hi(text: &str) -> Option<(Site, String)> {
    let mut best: Option<(usize, Site, String)> = None;

    for (site, re) in TROLL.iter() {
        if let Some(m) = re.find(text) {
            let url = helper(m.as_str()).to_string();
            match best {
                None => best = Some((m.start(), *site, url)),
                Some((best_i, _, _)) if m.start() < best_i => best = Some((m.start(), *site, url)),
                _ => {}
            }
        }
    }

    best.map(|(_, s, u)| (s, u))
}

fn embed(site: Site, url: &str) -> String {
    let rest = url.splitn(2, "://").nth(1).unwrap_or(url);
    let path = rest.find('/').map(|i| &rest[i..]).unwrap_or("");

    match site {
        Site::Twitter => format!("https://fixvx.com{}", path),
        Site::Instagram => format!("https://kkinstagram.com{}", path),
        Site::Reddit => format!("https://vxreddit.com{}", path),
        Site::TikTok => format!("https://d.tnktok.com{}", path),
    }
}

static HELLO: Lazy<Regex> = Lazy::new(|| {Regex::new(r"(?i)https?://(?:(?:www\.)?(?:x|twitter)\.com|(?:www\.)?(?:instagram\.com|instagr\.am)|(?:(?:old|new|www)\.)?reddit\.com|redd\.it|(?:(?:vt|m|www)\.)?tiktok\.com)(?:/[^\s]*)?").unwrap()});

static TROLL: Lazy<Vec<(Site, Regex)>> = Lazy::new(|| {
    vec![
        (Site::Twitter,   Regex::new(r"(?i)https?://(?:(?:www\.)?(?:x|twitter)\.com)(?:/[^\s]*)?").unwrap()),
        (Site::Instagram, Regex::new(r"(?i)https?://(?:(?:www\.)?(?:instagram\.com|instagr\.am))(?:/[^\s]*)?").unwrap()),
        (Site::Reddit,    Regex::new(r"(?i)https?://(?:(?:(?:old|new|www)\.)?reddit\.com|redd\.it)(?:/[^\s]*)?").unwrap()),
        (Site::TikTok,    Regex::new(r"(?i)https?://(?:(?:(?:vt|m|www)\.)?tiktok\.com)(?:/[^\s]*)?").unwrap()),
    ]
});



struct Handler;

#[serenity::async_trait]
impl EventHandler for Handler {
    async fn message(&self, _ctx: Context, msg: Message) {
        if HELLO.is_match(&msg.content) {
            if let Err(err) = msg.react(&_ctx.http, ReactionType::Unicode("🇪".into())).await {
                eprintln!("[message] react error msg_id={} err={:?}", msg.id, err);
                return;
            }

            if let Some((site, url)) = hi(&msg.content) {
                let rewritten = embed(site, &url);
                anti_troll_measure.lock().await.insert(msg.id, rewritten);
            }
        }
    }

    async fn reaction_add(&self, ctx: Context, rxn: Reaction) {
        let ReactionType::Unicode(ref s) = rxn.emoji else { return; };
        if s != "🇪" { return; }

        let maybe_link = {anti_troll_measure.lock().await.get(&rxn.message_id).cloned() };
        if maybe_link.is_none() { return; }

        let Ok(users) = rxn.channel_id
            .reaction_users(&ctx.http, rxn.message_id, ReactionType::Unicode("🇪".into()), Some(100), None)
            .await else {
                eprintln!("[reaction_add] reaction_users failed msg_id={}", rxn.message_id);
                return;
            };

        let me_id = ctx.cache.current_user().id;
        let bot_reacted = users.iter().any(|u| u.id == me_id);
        let sdf = users.iter().count();

        if !bot_reacted {
            let _ = rxn.channel_id.create_reaction(
                &ctx.http, rxn.message_id, ReactionType::Unicode("🇪".into())
            ).await;
            return;
        }

        if sdf < 2 {
            return;
        }

        {
            let mut temp = SENT.lock().await;
            if temp.contains(&rxn.message_id) {return;}
            temp.insert(rxn.message_id);
        }

        let temp_link = &maybe_link;

        if let Err(err) = rxn.channel_id.say(&ctx.http, temp_link.as_ref().unwrap().clone()).await {
            eprint!("{:?}", err);
            SENT.lock().await.remove(&rxn.message_id);
            return;
        };

        sleep(Duration::from_millis(600)).await;

        if let Err(err) = rxn.channel_id.edit_message(&ctx.http, rxn.message_id, EditMessage::new().suppress_embeds(true)).await {
            sleep(Duration::from_millis(1200)).await;
            // there is a small chance Discord hasn’t yet fully parsed the contained links and generated the embeds
            if let Err (err2) = rxn.channel_id.edit_message(&ctx.http, rxn.message_id, EditMessage::new().suppress_embeds(true)).await {
                eprintln!("ERROR {:?} {:?}", err, err2);

            }
            
        }
        anti_troll_measure.lock().await.remove(&rxn.message_id);
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let token = std::env::var("DISCORD_TOKEN")?;
    let intents = GatewayIntents::GUILD_MESSAGES
        .union(GatewayIntents::MESSAGE_CONTENT)
        .union(GatewayIntents::GUILD_MEMBERS).union(GatewayIntents::GUILD_MESSAGE_REACTIONS);
    let mut client = Client::builder(&token, intents)
        .event_handler(Handler)
        .await?;
    client.start().await?;

    Ok(())
}
