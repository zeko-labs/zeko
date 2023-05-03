use async_graphql::{http, InputObject, Object, Result};
use async_graphql_poem::GraphQL;
use mina_hasher::Hashable;
use mina_signer::PubKey;
use o1_utils::FieldHelpers;
use poem::{get, handler, listener::TcpListener, web, IntoResponse, Route, Server};

use crate::{
    da_layer::DALayer,
    mina_daemon::{validate_payment, MinaDaemon},
    transaction::{self, Transaction},
};

struct Context {
    mina_daemon_client: MinaDaemon,
    da_layer_client: DALayer,
}

#[derive(InputObject)]
struct SignatureInput {
    scalar: String,
    field: String,
}

#[derive(InputObject)]
struct SendPaymentInput {
    nonce: u32,
    valid_until: u32,
    fee: u64,
    amount: u64,
    to: String,
    from: String,
    memo: Option<String>,
}

pub struct Query;

#[Object]
impl Query {
    async fn ping(&self) -> &'static str {
        "pong"
    }
}

pub struct Mutation;

#[Object]
impl Mutation {
    async fn send_payment<'ctx>(
        &self,
        ctx: &async_graphql::Context<'ctx>,
        signature: SignatureInput,
        input: SendPaymentInput,
    ) -> Result<bool> {
        let ctx = ctx.data::<Context>().expect("Context should be set");

        let payment_valid = ctx
            .mina_daemon_client
            .validate_payment(
                validate_payment::SignatureInput {
                    field: signature.field.into(),
                    scalar: signature.scalar.into(),
                    raw_signature: None,
                }
                .into(),
                validate_payment::SendPaymentInput {
                    nonce: Some(input.nonce),
                    memo: input.memo.clone(),
                    valid_until: Some(input.valid_until),
                    fee: input.fee,
                    amount: input.amount,
                    to: input.to.clone(),
                    from: input.from.clone(),
                },
            )
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;

        if !payment_valid {
            return Err(async_graphql::Error::new("Payment is invalid"));
        }

        let mut memo = input.memo.unwrap_or_default().as_bytes().to_vec();
        memo.resize(transaction::MEMO_BYTES, 0);

        let transaction = Transaction::new_payment(
            PubKey::from_address(&input.from)?,
            PubKey::from_address(&input.to)?,
            input.amount,
            input.fee,
            input.nonce,
            input.valid_until,
            memo.try_into().expect("Memo should be 34 bytes"),
        );

        let transaction_fields = transaction
            .to_roinput()
            .to_fields()
            .iter()
            .map(|field| {
                field
                    .to_bytes()
                    .try_into()
                    .expect("Field should be 32 bytes")
            })
            .collect::<Vec<[u8; 32]>>();

        ctx.da_layer_client
            .post_batch(transaction_fields)
            .await
            .map_err(|e| {
                async_graphql::Error::new(format!("Failed to post batch: {}", e.to_string()))
            })?;

        Ok(true)
    }
}

#[handler]
pub async fn graphiql() -> impl IntoResponse {
    web::Html(http::GraphiQLSource::build().endpoint("/graphql").finish())
}

pub async fn run(
    port: u16,
    mina_daemon_client: MinaDaemon,
    da_layer_client: DALayer,
) -> Result<(), std::io::Error> {
    let schema = async_graphql::Schema::build(Query, Mutation, async_graphql::EmptySubscription)
        .data(Context {
            mina_daemon_client,
            da_layer_client,
        })
        .finish();

    let app = Route::new().at("/graphql", get(graphiql).post(GraphQL::new(schema)));

    println!("Playground: http://localhost:{}/graphql", port);

    Server::new(TcpListener::bind(format!("0.0.0.0:{}", port)))
        .run(app)
        .await
}
