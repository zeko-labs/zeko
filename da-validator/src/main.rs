mod da_layer;
mod signer;

use clap::Parser;
use ethers::{
    providers::{Provider, Ws},
    signers::{LocalWallet, Signer},
};
use log::{info, Level, LevelFilter, Metadata, Record};
use mina_signer::SecKey;
use signer::FromBase58;
use std::error::Error;

use crate::da_layer::DALayer;

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    #[arg(long, env, default_value_t = String::from("ws://localhost:8546"))]
    da_websocket: String,

    #[arg(long, env, default_value_t = 1337)]
    da_chainid: u64,

    #[arg(long, env)]
    da_private_key: String,

    #[arg(long, env)]
    da_address: String,

    #[arg(long, env)]
    mina_private_key: String,

    #[arg(long, env, default_value_t = 0)]
    from_block: u64,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    log::set_logger(&Logger)
        .map(|()| log::set_max_level(LevelFilter::Info))
        .map_err(|_| "Failed to set logger")?;

    let provider = Provider::<Ws>::connect_with_reconnects(args.da_websocket, 10).await?;

    let wallet = args
        .da_private_key
        .parse::<LocalWallet>()?
        .with_chain_id(args.da_chainid);

    let da_layer_client = DALayer::new(
        provider,
        wallet,
        SecKey::from_base58(&args.mina_private_key)?,
        &args.da_address,
    )?;

    info!("Started listening to batches");

    da_layer_client.listen_to_batches(args.from_block).await?;

    Ok(())
}
