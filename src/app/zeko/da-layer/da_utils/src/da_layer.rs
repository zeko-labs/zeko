use ethers::{
    middleware::SignerMiddleware,
    prelude::abigen,
    providers::{Middleware, Provider, Ws},
    signers::{LocalWallet, Signer},
    types::{Address, TransactionReceipt, U256},
    utils::hex::ToHexExt,
};
use mina_curves::pasta::fields::{fp::Fp, fq::Fq};
use mina_signer::{BaseField, PubKey, ScalarField};
use o1_utils::{field_helpers::FieldHelpersError, FieldHelpers};
use std::{str::FromStr, sync::Arc};

// Generate bindings for the DA layer contract from the ABI
abigen!(DAContract, "src/DataAvailability.json");

// Wrap second contract in a module to avoid name collisions of types
mod da_proxy {
    use super::*;
    abigen!(Abi, "src/DataAvailabilityProxy.json");
}

pub trait IntoSolidityType<T> {
    fn into_solidity_type(self) -> T;
}

impl IntoSolidityType<MinaPublicKey> for PubKey {
    fn into_solidity_type(self) -> MinaPublicKey {
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
    fn into_solidity_type(self) -> [u8; 32] {
        let mut bytes = self.to_bytes();

        bytes.resize(32, 0);

        bytes.try_into().expect("field element is 32 bytes")
    }
}

impl IntoSolidityType<[u8; 32]> for Fp {
    fn into_solidity_type(self) -> [u8; 32] {
        let mut bytes = self.to_bytes();

        bytes.resize(32, 0);

        bytes.try_into().expect("field element is 32 bytes")
    }
}

// ethers-rs doesn't derive serde::Serialize trait for types generated from ABIs
mod serializable {
    use serde::Serialize;

    #[derive(Serialize)]
    pub struct MinaPublicKey {
        pub x: String,
        pub y: String,
    }

    #[derive(Serialize)]
    pub struct MinaSchnorrSignature {
        pub public_key: MinaPublicKey,
        pub rx: String,
        pub s: String,
    }
}

// Struct holding context for executing transactions on the DA layer
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
        let wallet = da_private_key
            .parse::<LocalWallet>()?
            .with_chain_id(provider.get_chainid().await?.as_u64());
        let client = SignerMiddleware::new(provider, wallet.clone());
        let contract = DAContract::new(da_contract_address.parse::<Address>()?, Arc::new(client));

        Ok(Self { wallet, contract })
    }

    // Deploys the DA layer contract and returns the address
    // DA contract is a proxy contract that forwards calls to the implementation contract
    pub async fn deploy(
        da_websocket: &str,
        da_private_key: &str,
        quorum: u64,
        validators: Vec<PubKey>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let provider = Provider::<Ws>::connect_with_reconnects(da_websocket, 10).await?;
        let wallet = da_private_key
            .parse::<LocalWallet>()?
            .with_chain_id(provider.get_chainid().await?.as_u64());
        let client = Arc::new(SignerMiddleware::new(provider, wallet.clone()));

        let implementation_contract = DAContract::deploy(client.clone(), ())?.send().await?;

        let proxy = da_proxy::Abi::deploy::<(Address, U256, Vec<MinaPublicKey>)>(
            client,
            (
                implementation_contract.address(),
                U256::from(quorum),
                validators
                    .iter()
                    .map(|pk| {
                        let point = pk.clone().into_point();
                        MinaPublicKey {
                            x: point
                                .x
                                .to_bytes()
                                .try_into()
                                .expect("Failed to convert BaseField to [u8; 32]"),
                            y: point
                                .y
                                .to_bytes()
                                .try_into()
                                .expect("Failed to convert BaseField to [u8; 32]"),
                        }
                    })
                    .collect(),
            ),
        )?
        .send()
        .await?;

        Ok(proxy.address().encode_hex_with_prefix())
    }

    pub async fn post_batch(
        &self,
        data: &str,
        sig_data_without_location: Vec<BaseField>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let sig_data_without_location = sig_data_without_location
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
            .post_batch(data.to_string(), sig_data_without_location)
            .from(self.wallet.address())
            .legacy();

        let pending_tx = txn.send().await?;

        let receipt: Option<TransactionReceipt> = pending_tx.await?;

        match receipt {
            Some(receipt) => {
                // We get the new location from the emmitted event (log)
                let log = receipt.logs.first().ok_or("No logs")?;
                // First topic is address of a contract, second topic is location
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

    pub async fn post_batch_signature(
        &self,
        location: &str,
        mina_pk: &str,
        sig_rx: BaseField,
        sig_s: ScalarField,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let signature = MinaSchnorrSignature {
            public_key: PubKey::from_address(mina_pk)?.into_solidity_type(),
            rx: sig_rx.into_solidity_type(),
            s: sig_s.into_solidity_type(),
        };

        let tx = self
            .contract
            .add_batch_signature(U256::from_str(location)?, signature)
            .from(self.wallet.address())
            .legacy();

        let pending_tx = tx.send().await?;

        let receipt = pending_tx.await?;

        match receipt {
            Some(_) => Ok(()),
            None => Err("Transaction failed".into()),
        }
    }
}

// Struct holding context for reading DA layer state
pub struct DALayerReader {
    contract: DAContract<Provider<Ws>>,
}

impl DALayerReader {
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

    pub async fn get_batch_signatures(
        &self,
        location: &str,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let signatures = self
            .contract
            .get_batch_signatures(U256::from_str(location)?)
            .await?;

        let serializable = signatures
            .iter()
            .map(|sig| {
                Ok(serializable::MinaSchnorrSignature {
                    public_key: serializable::MinaPublicKey {
                        x: BaseField::from_bytes(&sig.public_key.x)?
                            .to_biguint()
                            .to_string(),
                        y: BaseField::from_bytes(&sig.public_key.y)?
                            .to_biguint()
                            .to_string(),
                    },
                    rx: BaseField::from_bytes(&sig.rx)?.to_biguint().to_string(),
                    s: ScalarField::from_bytes(&sig.s)?.to_biguint().to_string(),
                })
            })
            .collect::<Result<Vec<serializable::MinaSchnorrSignature>, FieldHelpersError>>()?;

        Ok(serde_json::to_string(&serializable)?)
    }
}
