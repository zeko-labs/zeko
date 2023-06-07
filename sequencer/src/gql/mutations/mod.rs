use std::str::FromStr;

use async_graphql::{Object, Result};
use mina_curves::pasta::{Fp, Fq};
use mina_p2p_messages::v2::MinaBaseZkappCommandTStableV1WireStableV1;
use mina_signer::{CompressedPubKey, Signature};
use mina_tree::scan_state::{
    currency::{Amount, Fee, Nonce, Slot},
    transaction_logic::{
        signed_command::{Body, Common, PaymentPayload, SignedCommand, SignedCommandPayload},
        zkapp_command::ZkAppCommand,
        Memo,
    },
};

use super::{
    context::Context,
    types::{
        Account, PublicKey, SendPaymentInput, SendPaymentPayload, SendZkappPayload, SignatureInput,
        TokenId, TransactionHash, TransactionId, U_Int32, U_Int64, UserCommand, ZkappCommandInput,
    },
};

pub struct Mutation;

#[Object]
impl Mutation {
    async fn send_payment<'ctx>(
        &self,
        ctx: &async_graphql::Context<'ctx>,
        signature: SignatureInput,
        input: SendPaymentInput,
    ) -> Result<SendPaymentPayload> {
        let Context {
            da_layer_client: _,
            ledger,
        } = ctx.data::<Context>().expect("Context should be set");

        let mut ledger = ledger.lock().await;

        let from_pk = CompressedPubKey::from_address(&input.from)?;

        let source_acc = ledger
            .get_or_create_account(from_pk.clone(), None)
            .map_err(|e| {
                async_graphql::Error::new(format!(
                    "Error getting or creating account: {}",
                    e.to_string()
                ))
            })?;

        let receiver_acc = ledger
            .get_or_create_account(CompressedPubKey::from_address(&input.to)?, None)
            .map_err(|e| {
                async_graphql::Error::new(format!(
                    "Error getting or creating account: {}",
                    e.to_string()
                ))
            })?;

        let command = SignedCommand {
            signer: from_pk.clone(),
            signature: Signature {
                rx: Fp::from_str(&signature.field).map_err(|_| {
                    async_graphql::Error::new("Invalid signature field".to_string())
                })?,
                s: Fq::from_str(&signature.scalar).map_err(|_| {
                    async_graphql::Error::new("Invalid signature scalar".to_string())
                })?,
            },
            payload: SignedCommandPayload {
                common: Common {
                    fee: Fee::from_u64(input.fee.0.parse()?),
                    fee_payer_pk: from_pk.clone(),
                    nonce: Nonce::from_u32(input.nonce.0.parse()?),
                    valid_until: Slot::from_u32(input.valid_until.0.parse()?),
                    memo: Memo::dummy(),
                },
                body: Body::Payment(PaymentPayload {
                    source_pk: from_pk,
                    receiver_pk: CompressedPubKey::from_address(&input.to)?,
                    amount: Amount::from_u64(input.amount.0.parse()?),
                }),
            },
        };

        // TODO: validate signature

        let result = ledger.apply_payment(command.clone());

        match result {
            Ok(_) => Ok(SendPaymentPayload {
                payment: UserCommand {
                    id: TransactionId("".into()),
                    hash: TransactionHash("".into()),
                    kind: "Payment".into(),
                    nonce: U_Int32(command.nonce().as_u32().to_string()),
                    source: Account::from(source_acc.clone()),
                    receiver: Account::from(receiver_acc),
                    fee_payer: Account::from(source_acc),
                    valid_until: U_Int32(command.valid_until().as_u32().to_string()),
                    token: TokenId("".into()),
                    amount: U_Int64(match command.clone().payload.body {
                        Body::Payment(p) => p.amount.as_u64().to_string(),
                        _ => "0".into(),
                    }),
                    fee_token: TokenId(command.fee_token().0.to_string()),
                    fee: U_Int64(command.fee().as_u64().to_string()),
                    memo: "".into(),
                    failure_reason: None,

                    // deprecated
                    from: PublicKey(input.from),
                    to: PublicKey(input.to),
                    is_delegation: false,
                },
            }),
            Err(e) => Err(async_graphql::Error::new(e.to_string())),
        }
    }

    async fn send_zkapp<'ctx>(
        &self,
        ctx: &async_graphql::Context<'ctx>,
        zkapp_command: ZkappCommandInput,
    ) -> Result<SendZkappPayload> {
        let Context {
            da_layer_client: _,
            ledger,
        } = ctx.data::<Context>().expect("Context should be set");

        let mut ledger = ledger.lock().await;

        let json_zkapp_command = serde_json::to_string(&zkapp_command).unwrap();

        println!("json_zkapp_command: {}", json_zkapp_command);

        let p2p_zkapp_command =
            serde_json::from_str::<MinaBaseZkappCommandTStableV1WireStableV1>(&json_zkapp_command)
                .unwrap();

        let zkapp_command = ZkAppCommand::from(&p2p_zkapp_command);

        let result = ledger.apply_zkapp_command(zkapp_command);

        println!("result: {:?}", result);

        todo!()
    }
}
