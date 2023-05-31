use std::error::Error;

use graphql_client::{reqwest::post_graphql, GraphQLQuery};

type PublicKey = String;
type UInt32 = u32;
type UInt64 = u64;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/mina_daemon/schema.graphql",
    query_path = "src/mina_daemon/query.graphql",
    response_derives = "Debug"
)]
pub struct ValidatePayment;

pub struct MinaDaemon {
    pub url: String,
    pub client: reqwest::Client,
}

impl MinaDaemon {
    pub fn new(url: String) -> Self {
        Self {
            url,
            client: reqwest::Client::new(),
        }
    }

    pub async fn validate_payment(
        &self,
        signature: Option<validate_payment::SignatureInput>,
        input: validate_payment::SendPaymentInput,
    ) -> Result<bool, Box<dyn Error>> {
        let res = post_graphql::<ValidatePayment, _>(
            &self.client,
            &self.url,
            validate_payment::Variables { signature, input },
        )
        .await?;

        if let Some(errors) = res.errors {
            return Err(errors
                .iter()
                .map(|e| e.message.clone())
                .collect::<Vec<_>>()
                .join("\n")
                .into());
        }

        match res.data {
            Some(data) => Ok(data.validate_payment),
            None => Err("No data returned".into()),
        }
    }
}
