use ethers::{
    prelude::abigen,
    providers::{Provider, Ws},
    types::{Address, U256},
};
use std::{str::FromStr, sync::Arc};

abigen!(DAContract, "src/DataAvailability.json");

pub async fn get_batch_data(
    da_websocket: &str,
    da_contract_address: &str,
    location: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let provider = Provider::<Ws>::connect_with_reconnects(da_websocket, 10).await?;

    let contract = DAContract::new(da_contract_address.parse::<Address>()?, Arc::new(provider));

    let data = contract.get_batch_data(U256::from_str(location)?).await?;

    println!("Data: {:?}", data);

    Ok(())
}
