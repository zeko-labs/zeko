use ethers::{
    prelude::{abigen, SignerMiddleware},
    providers::{Provider, StreamExt, Ws},
    signers::{LocalWallet, Signer},
    types::{Address, TransactionReceipt, U256},
    utils::hex,
};
use log::{info, warn};
use mina_curves::pasta::fields::{fp::Fp, fq::Fq};
use mina_signer::{Keypair, PubKey, SecKey};
use o1_utils::FieldHelpers;
use std::sync::Arc;
use tokio::try_join;

use crate::signer::{sign, Message, NetworkId};

pub trait IntoSolidityType<T> {
    fn into_solidity_type(&self) -> T;
}

impl IntoSolidityType<MinaPublicKey> for PubKey {
    fn into_solidity_type(&self) -> MinaPublicKey {
        let point = self.point();

        let mut x_bytes = point.x.to_bytes();
        let mut y_bytes = point.y.to_bytes();

        x_bytes.resize(32, 0);
        y_bytes.resize(32, 0);

        MinaPublicKey {
            x: x_bytes.try_into().expect("x coordinate is 32 bytes"),
            y: y_bytes.try_into().expect("y coordinate is 32 bytes"),
        }
    }
}

impl IntoSolidityType<[u8; 32]> for Fq {
    fn into_solidity_type(&self) -> [u8; 32] {
        let mut bytes = self.to_bytes();

        bytes.resize(32, 0);

        bytes.try_into().expect("field element is 32 bytes")
    }
}

impl IntoSolidityType<[u8; 32]> for Fp {
    fn into_solidity_type(&self) -> [u8; 32] {
        let mut bytes = self.to_bytes();

        bytes.resize(32, 0);

        bytes.try_into().expect("field element is 32 bytes")
    }
}

abigen!(DAContract, "src/da_layer/DataAvailability.json");

pub struct DALayer {
    pub client: SignerMiddleware<Provider<Ws>, LocalWallet>,
    pub contract: DAContract<SignerMiddleware<Provider<Ws>, LocalWallet>>,
    pub mina_keypair: Keypair,
    pub mina_network_id: NetworkId,
}

impl DALayer {
    pub fn new(
        ws_provider: Provider<Ws>,
        wallet: LocalWallet,
        mina_sec_key: SecKey,
        da_contract_address: &str,
        mina_network_id: NetworkId,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let client = SignerMiddleware::new(ws_provider, wallet);

        let contract = DAContract::new(
            da_contract_address.parse::<Address>()?,
            Arc::new(client.clone()),
        );

        let mina_keypair = Keypair::from_secret_key(mina_sec_key)?;

        Ok(Self {
            client,
            contract,
            mina_keypair,
            mina_network_id,
        })
    }

    pub async fn listen_to_batches(
        &self,
        from_block: u64,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let events = self
            .contract
            .event::<BatchProposedFilter>()
            .from_block(from_block);

        let mut stream = events.subscribe().await?;

        while let Some(Ok(e)) = stream.next().await {
            let BatchProposedFilter { batch_id } = e;

            if let Err(e) = self.process_batch(batch_id).await {
                warn!(
                    "Error processing batch {:?} with error: {:?}",
                    hex::encode(&batch_id),
                    e
                );
            }
        }

        Ok(())
    }

    pub async fn process_batch(
        &self,
        batch_id: [u8; 32],
    ) -> Result<Option<TransactionReceipt>, Box<dyn std::error::Error>> {
        let signatures_call = self.contract.get_batch_signatures(batch_id);
        let data_call = self.contract.get_batch_fields(batch_id);
        let quorum_call = self.contract.quorum();

        let (signatures, data, quorum): (Vec<MinaSchnorrSignature>, Vec<[u8; 32]>, U256) =
            try_join!(signatures_call.call(), data_call.call(), quorum_call.call())?;

        if signatures.len() >= quorum.as_usize()
            || signatures
                .iter()
                .any(|s| s.public_key == self.mina_keypair.public.into_solidity_type())
        {
            info!("Skipping batch: {:?}", hex::encode(&batch_id));
            return Ok(None);
        }

        let msg = Message::from_bytes_slice(&data)?;

        let signature = sign(&self.mina_keypair, &msg, self.mina_network_id.clone());

        let signature = MinaSchnorrSignature {
            public_key: self.mina_keypair.public.clone().into_solidity_type(),
            rx: signature.rx.into_solidity_type(),
            s: signature.s.into_solidity_type(),
        };

        let tx = self
            .contract
            .add_batch_signature(batch_id, signature)
            .from(self.client.signer().address())
            .legacy();

        let pending_tx = tx.send().await?;

        let receipt = pending_tx.await?;

        info!("Signed batch: {:?}", hex::encode(&batch_id));

        match receipt {
            Some(receipt) => Ok(Some(receipt)),
            None => Err("Transaction failed".into()),
        }
    }
}
