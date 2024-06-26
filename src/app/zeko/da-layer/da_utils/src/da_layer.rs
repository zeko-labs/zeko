use ethers::{
    prelude::abigen,
    providers::{Provider, Ws},
    types::{Address, Bytes, U256},
};
use std::{str::FromStr, sync::Arc};

abigen!(DAContract, "src/DataAvailability.json");

pub struct DALayer<Client> {
    contract: DAContract<Provider<Client>>,
}

impl DALayer<Ws> {
    pub async fn new(
        da_websocket: &str,
        da_contract_address: &str,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let provider = Provider::<Ws>::connect_with_reconnects(da_websocket, 10).await?;
        let contract = DAContract::new(da_contract_address.parse::<Address>()?, Arc::new(provider));

        Ok(Self { contract })
    }

    pub async fn get_batch_data(
        &self,
        location: &str,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let (data, _) = self
            .contract
            .get_batch_data(U256::from_str(location)?)
            .await?;

        Ok(Bytes::to_string(&data))
    }
}
