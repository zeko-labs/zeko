mod da_layer;
mod gql;
mod mina_daemon;
mod transaction;

use std::error::Error;

use clap::Parser;
use da_layer::DALayer;
use mina_daemon::MinaDaemon;

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    #[arg(short, long, env, default_value_t = 8000)]
    port: u16,

    #[arg(long, env)]
    mina_daemon_url: String,

    #[arg(long, env, default_value_t = String::from("http://localhost:8545"))]
    da_provider: String,

    #[arg(long, env, default_value_t = 1337)]
    da_chainid: u64,

    #[arg(long, env)]
    da_private_key: String,

    #[arg(long, env)]
    da_address: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    gql::run(
        args.port,
        MinaDaemon::new(args.mina_daemon_url),
        DALayer::new(
            args.da_provider,
            args.da_chainid,
            args.da_private_key,
            args.da_address,
        )?,
    )
    .await?;

    Ok(())
}
