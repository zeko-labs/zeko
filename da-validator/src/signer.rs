use std::error::Error;

use mina_hasher::{DomainParameter, Hashable, ROInput};
use mina_signer::{BaseField, Keypair, SecKey, Signature, Signer};
use o1_utils::{field_helpers::FieldHelpersError, FieldHelpers};
use sha2::{Digest, Sha256};

pub const MINA_SEC_KEY_LEN: usize = 52;

pub trait FromBase58 {
    fn from_base58(address: &str) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized;
}

impl FromBase58 for SecKey {
    fn from_base58(base58: &str) -> Result<Self, Box<dyn Error>> {
        if base58.len() != MINA_SEC_KEY_LEN {
            return Err("Invalid secret key length".into());
        }

        let bytes = bs58::decode(base58).into_vec()?;

        let (raw, checksum) = (&bytes[..bytes.len() - 4], &bytes[bytes.len() - 4..]);

        let hash = Sha256::digest(&Sha256::digest(raw)[..]);

        if checksum != &hash[..4] {
            return Err("Invalid checksum".into());
        }

        let (version, scalar_bytes) = (&raw[..2], &raw[2..raw.len()]);

        if version != [0x5a, 0x01] {
            return Err("Invalid version".into());
        }

        let mut scalar_bytes = scalar_bytes.to_vec();

        scalar_bytes.reverse();

        Ok(Self::from_bytes(&scalar_bytes)?)
    }
}

#[derive(Debug, Clone)]
pub enum NetworkId {
    MAINNET = 0x00,
    TESTNET = 0x01,
    NULLNET = 0x02,
}

impl TryFrom<u8> for NetworkId {
    type Error = Box<dyn Error>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(NetworkId::MAINNET),
            0x01 => Ok(NetworkId::TESTNET),
            0x02 => Ok(NetworkId::NULLNET),
            _ => Err("Invalid network id".into()),
        }
    }
}

impl From<NetworkId> for u8 {
    fn from(id: NetworkId) -> u8 {
        id as u8
    }
}

impl DomainParameter for NetworkId {
    fn into_bytes(self) -> Vec<u8> {
        vec![self as u8]
    }
}

#[derive(Clone)]
pub struct Message {
    pub fields: Vec<BaseField>,
}

impl Message {
    pub fn from_bytes_slice(fields_bytes: &[[u8; 32]]) -> Result<Self, FieldHelpersError> {
        Ok(Self {
            fields: fields_bytes
                .iter()
                .map(|bytes| BaseField::from_bytes(bytes))
                .collect::<Result<Vec<BaseField>, FieldHelpersError>>()?,
        })
    }
}

impl Hashable for Message {
    type D = NetworkId;

    fn to_roinput(&self) -> ROInput {
        self.fields
            .iter()
            .fold(ROInput::new(), |roi, field| roi.append_field(*field))
    }

    fn domain_string(network_id: NetworkId) -> Option<String> {
        match network_id {
            NetworkId::MAINNET => "MinaSignatureMainnet".to_string().into(),
            NetworkId::TESTNET => "CodaSignature".to_string().into(),
            NetworkId::NULLNET => None,
        }
    }
}

pub fn sign(kp: &Keypair, msg: &Message, network_id: NetworkId) -> Signature {
    let mut signer = mina_signer::create_kimchi::<Message>(network_id);

    signer.sign(kp, msg)
}
