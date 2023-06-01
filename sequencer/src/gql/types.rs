use async_graphql::{
    scalar, InputObject, InputValueError, InputValueResult, Scalar, ScalarType, SimpleObject, Value,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct PublicKey(pub String);
scalar!(PublicKey);

#[derive(Serialize, Deserialize)]
pub struct Signature(pub String);
scalar!(Signature);

#[derive(Serialize, Deserialize)]
pub struct Field(pub String);
scalar!(Field);

#[derive(Serialize, Deserialize)]
pub struct TokenId(pub String);
scalar!(TokenId);

#[derive(Serialize, Deserialize)]
pub struct TransactionId(pub String);
scalar!(TransactionId);

#[derive(Serialize, Deserialize)]
pub struct TransactionHash(pub String);
scalar!(TransactionHash);

#[allow(non_camel_case_types)]
pub struct U_Int64(pub u64);

#[Scalar]
impl ScalarType for U_Int64 {
    fn parse(value: Value) -> InputValueResult<Self> {
        match &value {
            Value::Number(value) => Ok(U_Int64(value.as_u64().unwrap_or(0))),
            Value::String(value) => Ok(value.parse().map(U_Int64)?),
            _ => Err(InputValueError::expected_type(value)),
        }
    }

    fn to_value(&self) -> Value {
        Value::String(self.0.to_string())
    }
}

#[allow(non_camel_case_types)]
pub struct U_Int32(pub u32);

#[Scalar]
impl ScalarType for U_Int32 {
    fn parse(value: Value) -> InputValueResult<Self> {
        match &value {
            Value::Number(value) => Ok(U_Int32(value.as_u64().unwrap_or(0) as u32)),
            Value::String(value) => Ok(value.parse().map(U_Int32)?),
            _ => Err(InputValueError::expected_type(value)),
        }
    }

    fn to_value(&self) -> Value {
        Value::String(self.0.to_string())
    }
}

#[derive(InputObject)]
pub struct SignatureInput {
    pub scalar: String,
    pub field: String,
}

#[derive(InputObject)]
pub struct SendPaymentInput {
    pub nonce: U_Int32,
    pub valid_until: U_Int32,
    pub fee: U_Int64,
    pub amount: U_Int64,
    pub to: String,
    pub from: String,
    pub memo: Option<String>,
}

#[derive(SimpleObject)]
pub struct AnnotatedBalance {
    pub total: String,
    pub unknown: String,
    pub block_height: String,
}

#[derive(SimpleObject)]
pub struct AccountTiming {
    pub initial_minimum_balance: Option<String>,
    pub cliff_time: Option<String>,
    pub cliff_amount: Option<String>,
    pub vesting_period: Option<String>,
    pub vesting_increment: Option<String>,
}

#[derive(SimpleObject)]
pub struct Account {
    pub public_key: PublicKey,
    pub token_id: String,
    pub timing: AccountTiming,
    pub balance: AnnotatedBalance,
    pub nonce: U_Int64,
    pub inferred_nonce: U_Int64,
    pub delegate: Option<PublicKey>,
    pub delegate_account: Option<Box<Account>>,
    pub staking_active: bool,
    pub receipt_chain_hash: Option<String>,
    pub token_symbol: Option<String>,
}

impl From<mina_tree::Account> for Account {
    fn from(account: mina_tree::Account) -> Self {
        Account {
            public_key: PublicKey(account.public_key.into_address()),
            token_id: account.token_id.0.to_string(),
            timing: AccountTiming {
                initial_minimum_balance: None,
                cliff_time: None,
                cliff_amount: None,
                vesting_period: None,
                vesting_increment: None,
            },
            balance: AnnotatedBalance {
                total: account.balance.as_u64().to_string(),
                unknown: "0".into(),
                block_height: "0".into(),
            },
            nonce: U_Int64(account.nonce.as_u32() as u64),
            inferred_nonce: U_Int64(account.nonce.as_u32() as u64),
            delegate: None,
            delegate_account: None,
            staking_active: false,
            receipt_chain_hash: None,
            token_symbol: Some(account.token_symbol.0),
        }
    }
}

#[derive(SimpleObject)]
pub struct UserCommand {
    pub id: TransactionId,
    pub hash: TransactionHash,
    pub kind: String,
    pub nonce: U_Int32,
    pub source: Account,
    pub receiver: Account,
    pub fee_payer: Account,
    pub valid_until: U_Int32,
    pub token: TokenId,
    pub amount: U_Int64,
    pub fee_token: TokenId,
    pub fee: U_Int64,
    pub memo: String,
    pub failure_reason: Option<String>,

    // deprecated
    pub from: PublicKey,
    pub to: PublicKey,
    pub is_delegation: bool,
}

#[derive(SimpleObject)]
pub struct SendPaymentPayload {
    pub payment: UserCommand,
}

#[derive(InputObject)]
pub struct FeePayerBodyInput {
    pub public_key: PublicKey,
    pub fee: U_Int64,
    pub valid_until: U_Int32,
    pub nonce: U_Int32,
}

#[derive(InputObject)]
pub struct ZkappFeePayerInput {
    pub body: FeePayerBodyInput,
    pub authorization: Signature,
}

#[derive(InputObject)]
pub struct VerificationKeyWithHashInput {
    pub data: String,
    pub hash: Field,
}

#[derive(InputObject)]
pub struct PermissionsInput {
    pub edit_state: String,
    pub access: String,
    pub send: String,
    pub receive: String,
    pub set_delegate: String,
    pub set_permissions: String,
    pub set_verification_key: String,
    pub set_zkapp_uri: String,
    pub edit_action_state: String,
    pub set_token_symbol: String,
    pub increment_nonce: String,
    pub set_voting_for: String,
    pub set_timing: String,
}

#[derive(InputObject)]
pub struct TimingInput {
    pub initial_minimum_balance: String,
    pub cliff_time: String,
    pub cliff_amount: String,
    pub vesting_period: String,
    pub vesting_increment: String,
}

#[derive(InputObject)]
pub struct AccountUpdateModificationInput {
    pub app_state: Vec<Field>,
    pub delegate: Option<PublicKey>,
    pub verification_key: Option<VerificationKeyWithHashInput>,
    pub permissions: Option<PermissionsInput>,
    pub zkapp_uri: Option<String>,
    pub token_symbol: Option<String>,
    pub timing: Option<TimingInput>,
    pub voting_for: Option<String>,
}

#[derive(InputObject)]
pub struct BalanceChangeInput {
    pub magnitude: String,
    pub sgn: String,
}

#[derive(InputObject)]
pub struct LengthIntervalInput {
    pub lower: U_Int32,
    pub upper: U_Int32,
}

#[derive(InputObject)]
pub struct EpochLedgerPreconditionInput {
    pub hash: Field,
    pub total_currency: Option<LengthIntervalInput>,
}

#[derive(InputObject)]
pub struct EpochDataPreconditionInput {
    pub ledger: EpochLedgerPreconditionInput,
    pub seed: Option<Field>,
    pub start_checkpoint: Option<Field>,
    pub lock_checkpoint: Option<Field>,
    pub epoch_length: Option<LengthIntervalInput>,
}

#[derive(InputObject)]
pub struct NetworkPreconditionInput {
    pub snarked_ledger_hash: Option<Field>,
    pub blockchain_length: Option<LengthIntervalInput>,
    pub min_window_density: Option<LengthIntervalInput>,
    pub total_currency: Option<LengthIntervalInput>,
    pub global_slot_since_genesis: Option<LengthIntervalInput>,
    pub staking_epoch_data: EpochDataPreconditionInput,
    pub next_epoch_data: EpochDataPreconditionInput,
}

#[derive(InputObject)]
pub struct AccountPreconditionInput {
    pub balance: Option<LengthIntervalInput>,
    pub nonce: Option<LengthIntervalInput>,
    pub receipt_chain_hash: Option<Field>,
    pub delegate: Option<PublicKey>,
    pub state: Vec<Field>,
    pub action_state: Option<Field>,
    pub proved_state: Option<bool>,
    pub is_new: Option<bool>,
}

#[derive(InputObject)]
pub struct PreconditionsInput {
    pub network: NetworkPreconditionInput,
    pub account: AccountPreconditionInput,
    pub valid_while: Option<LengthIntervalInput>,
}

#[derive(InputObject)]
pub struct MayUseTokenInput {
    pub parents_own_token: bool,
    pub inherit_from_parent: bool,
}

#[derive(InputObject)]
pub struct AuthorizationKindStructuredInput {
    pub is_signed: bool,
    pub is_proved: bool,
    pub verification_key_hash: Field,
}

#[derive(InputObject)]
pub struct AccountUpdateBodyInput {
    pub public_key: PublicKey,
    pub token_id: TokenId,
    pub update: AccountUpdateModificationInput,
    pub balance_change: BalanceChangeInput,
    pub increment_nonce: bool,
    pub events: Vec<Vec<Field>>,
    pub actions: Vec<Vec<Field>>,
    pub call_data: Field,
    pub call_depth: i64,
    pub preconditions: PreconditionsInput,
    pub use_full_commitment: bool,
    pub implicit_account_creation_fee: bool,
    pub may_use_token: MayUseTokenInput,
    pub authorization_kind: AuthorizationKindStructuredInput,
}

#[derive(InputObject)]
pub struct ControlInput {
    pub proof: Option<String>,
    pub signature: Option<Signature>,
}

#[derive(InputObject)]
pub struct ZkappAccountUpdateInput {
    pub body: AccountUpdateBodyInput,
    pub authorization: ControlInput,
}

#[derive(InputObject)]
pub struct ZkappCommandInput {
    pub fee_payer: ZkappFeePayerInput,
    pub account_updates: Vec<ZkappAccountUpdateInput>,
    pub memo: String,
}

#[derive(InputObject)]
pub struct SendZkappInput {
    pub zkapp_command: ZkappCommandInput,
}

#[derive(SimpleObject)]
pub struct ZkappCommandResult {
    pub id: TransactionId,
    pub hash: TransactionHash,
    pub zkapp_command: String,
    pub failure_reason: Vec<String>,
}

#[derive(SimpleObject)]
pub struct SendZkappPayload {
    pub zkapp: ZkappCommandResult,
}

#[derive(SimpleObject)]
pub struct DaemonStatus {
    pub chain_id: String,
}
