use std::sync::Arc;

use ethers::{
    prelude::{abigen, SignerMiddleware},
    providers::{Http, Provider},
    signers::{LocalWallet, Signer},
    types::{Address, TransactionReceipt},
};

abigen!(DAContract, "src/da_layer/DataAvailability.json");

pub struct DALayer {
    pub client: SignerMiddleware<Provider<Http>, LocalWallet>,
    pub contract: DAContract<SignerMiddleware<Provider<Http>, LocalWallet>>,
}

impl DALayer {
    pub fn new(
        provider_url: String,
        chain_id: u64,
        signer_private_key: String,
        da_contract_address: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let provider = Provider::<Http>::try_from(provider_url)?;

        let wallet = signer_private_key
            .parse::<LocalWallet>()?
            .with_chain_id(chain_id);

        let client = SignerMiddleware::new(provider, wallet);

        let contract = DAContract::new(
            da_contract_address.parse::<Address>()?,
            Arc::new(client.clone()),
        );

        Ok(Self { client, contract })
    }

    pub async fn post_batch(
        &self,
        fields: Vec<[u8; 32]>,
    ) -> Result<TransactionReceipt, Box<dyn std::error::Error>> {
        let tx = self
            .contract
            .propose_batch(fields)
            .from(self.client.signer().address())
            .legacy();

        let pending_tx = tx.send().await?;

        let receipt = pending_tx.await?;

        match receipt {
            Some(receipt) => Ok(receipt),
            None => Err("Transaction failed".into()),
        }
    }
}
