use std::str::FromStr;

use async_graphql::{Object, Result};
use mina_curves::pasta::{Fp, Fq};
use mina_p2p_messages::v2::MinaBaseSignatureStableV1;
use mina_signer::{CompressedPubKey, Signature};
use mina_tree::scan_state::{
    currency::{Amount, Fee, Nonce, Slot},
    transaction_logic::{
        signed_command::{Body, Common, PaymentPayload, SignedCommand, SignedCommandPayload},
        zkapp_command::{FeePayer, FeePayerBody, ZkAppCommand},
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
                    fee: Fee::from_u64(input.fee.0),
                    fee_payer_pk: from_pk.clone(),
                    nonce: Nonce::from_u32(input.nonce.0),
                    valid_until: Slot::from_u32(input.valid_until.0),
                    memo: Memo::dummy(),
                },
                body: Body::Payment(PaymentPayload {
                    source_pk: from_pk,
                    receiver_pk: CompressedPubKey::from_address(&input.to)?,
                    amount: Amount::from_u64(input.amount.0),
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
                    nonce: U_Int32(command.nonce().as_u32()),
                    source: Account::from(source_acc.clone()),
                    receiver: Account::from(receiver_acc),
                    fee_payer: Account::from(source_acc),
                    valid_until: U_Int32(command.valid_until().as_u32()),
                    token: TokenId("".into()),
                    amount: U_Int64(match command.clone().payload.body {
                        Body::Payment(p) => p.amount.as_u64(),
                        _ => 0,
                    }),
                    fee_token: TokenId(command.fee_token().0.to_string()),
                    fee: U_Int64(command.fee().as_u64()),
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

        // let mut memo = input.memo.unwrap_or_default().as_bytes().to_vec();
        // memo.resize(transaction::MEMO_BYTES, 0);

        // let transaction = Transaction::new_payment(
        //     PubKey::from_address(&input.from)?,
        //     PubKey::from_address(&input.to)?,
        //     input.amount,
        //     input.fee,
        //     input.nonce,
        //     input.valid_until,
        //     memo.try_into().expect("Memo should be 34 bytes"),
        // );

        // let transaction_fields = transaction
        //     .to_roinput()
        //     .to_fields()
        //     .iter()
        //     .map(|field| {
        //         field
        //             .to_bytes()
        //             .try_into()
        //             .expect("Field should be 32 bytes")
        //     })
        //     .collect::<Vec<[u8; 32]>>();

        // ctx.da_layer_client
        //     .post_batch(transaction_fields)
        //     .await
        //     .map_err(|e| {
        //         async_graphql::Error::new(format!("Failed to post batch: {}", e.to_string()))
        //     })?;
    }

    // async fn send_zkapp<'ctx>(
    //     &self,
    //     ctx: &async_graphql::Context<'ctx>,
    //     zkapp_command: ZkappCommandInput,
    // ) -> Result<SendZkappPayload> {
    //     let Context {
    //         da_layer_client: _,
    //         ledger,
    //     } = ctx.data::<Context>().expect("Context should be set");

    //     let mut ledger = ledger.lock().await;

    //     let command = ZkAppCommand {
    //         fee_payer: FeePayer {
    //             body: FeePayerBody {
    //                 public_key: CompressedPubKey::from_address(
    //                     &zkapp_command.fee_payer.body.public_key.0,
    //                 )?,
    //                 fee: Fee::from_u64(zkapp_command.fee_payer.body.fee.0),
    //                 valid_until: Some(Slot::from_u32(zkapp_command.fee_payer.body.valid_until.0)),
    //                 nonce: Nonce::from_u32(zkapp_command.fee_payer.body.nonce.0),
    //             },
    //             authorization: MinaBaseSignatureStableV1::
    //         },
    //         memo: Memo::dummy(),
    //     };

    //     let result = ledger.apply_zkapp_command(command);

    //     todo!()
    // }
}
