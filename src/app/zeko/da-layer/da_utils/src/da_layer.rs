use ethers::{
    middleware::SignerMiddleware,
    prelude::abigen,
    providers::{Middleware, Provider, Ws},
    signers::{LocalWallet, Signer},
    types::{Address, TransactionReceipt, U256},
};
use mina_signer::BaseField;
use o1_utils::FieldHelpers;
use std::{str::FromStr, sync::Arc};

abigen!(DAContract, "src/DataAvailability.json");

pub struct DALayerExecutor {
    wallet: LocalWallet,
    contract: DAContract<SignerMiddleware<Provider<Ws>, LocalWallet>>,
}

impl DALayerExecutor {
    pub async fn new(
        da_websocket: &str,
        da_contract_address: &str,
        da_private_key: &str,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let provider = Provider::<Ws>::connect_with_reconnects(da_websocket, 10).await?;
        let chain_id = provider.get_chainid().await?.as_u64();
        let wallet = da_private_key
            .parse::<LocalWallet>()?
            .with_chain_id(chain_id);
        let client = SignerMiddleware::new(provider, wallet.clone());
        let contract = DAContract::new(da_contract_address.parse::<Address>()?, Arc::new(client));

        Ok(Self { wallet, contract })
    }

    pub async fn post_batch(
        &self,
        data: &str,
        sig_data: Vec<BaseField>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let sig_data = sig_data
            .iter()
            .map(BaseField::to_bytes)
            .map(|bytes| {
                bytes
                    .try_into()
                    .expect("Failed to convert BaseField to [u8; 32]")
            })
            .collect::<Vec<[u8; 32]>>();

        let txn = self
            .contract
            .post_batch(data.to_string(), sig_data)
            .from(self.wallet.address())
            .legacy();

        let pending_tx = txn.send().await?;

        let receipt: Option<TransactionReceipt> = pending_tx.await?;

        match receipt {
            Some(receipt) => {
                let log = receipt.logs.first().ok_or("No logs")?;
                // First topic is address, second topic is location
                let location = log.topics.get(1).ok_or("No location")?;

                Ok(location.to_low_u64_be().to_string())
            }
            None => Err("Transaction failed".into()),
        }
    }

    pub async fn init_genesis_state(&self, data: &str) -> Result<(), Box<dyn std::error::Error>> {
        let txn = self
            .contract
            .init_genesis_state(data.to_string())
            .from(self.wallet.address())
            .legacy();

        let pending_tx = txn.send().await?;

        let receipt: Option<TransactionReceipt> = pending_tx.await?;

        match receipt {
            Some(_) => Ok(()),
            None => Err("Transaction failed".into()),
        }
    }
}

pub struct DALayerCaller {
    contract: DAContract<Provider<Ws>>,
}

impl DALayerCaller {
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

        Ok(data)
    }

    pub async fn get_genesis_state(&self) -> Result<String, Box<dyn std::error::Error>> {
        let data = self.contract.genesis_state().await?;

        Ok(data)
    }
}
