mod da_layer;
mod gql;
mod ledger;

use clap::Parser;
use da_layer::DALayer;
use ledger::Ledger;
use mina_signer::CompressedPubKey;
use mina_tree::{scan_state::currency::Balance, Account, AccountId, TokenId};
use std::error::Error;

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    #[arg(short, long, env, default_value_t = 8000)]
    port: u16,

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

    let da_layer = DALayer::new(
        args.da_provider,
        args.da_chainid,
        args.da_private_key,
        args.da_address,
    )?;

    let genesis_accounts = vec![
        Account::create_with(
            AccountId::create(
                CompressedPubKey::from_address(
                    "B62qnPZzpnQWA8FLBn9qqJqPTeGuDdHTZgpmEMUNFCq8fWCRSqJS6Jd",
                )?,
                TokenId::default(),
            ),
            Balance::from_u64(1_000_000),
        ),
        Account::create_with(
            AccountId::create(
                CompressedPubKey::from_address(
                    "B62qrrytZmo8SraqYfJMZ8E3QcK77uAGZhsGJGKmVF5E598E8KX9j6a",
                )?,
                TokenId::default(),
            ),
            Balance::from_u64(100_000_000_000),
        ),
        Account::create_with(
            AccountId::create(
                CompressedPubKey::from_address(
                    "B62qjyadsumaW81YkokQkHLbzGSDGMt3DSbZMfyQpGzqQmMzteJGBCy",
                )?,
                TokenId::default(),
            ),
            Balance::from_u64(100_000_000_000),
        ),
    ];

    let ledger = Ledger::new(5, 1, genesis_accounts);

    gql::run(args.port, da_layer, ledger).await?;

    Ok(())
}
