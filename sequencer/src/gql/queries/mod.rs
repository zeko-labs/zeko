use async_graphql::{Object, Result};
use mina_p2p_messages::v2::MinaBaseZkappCommandTStableV1WireStableV1;
use mina_signer::CompressedPubKey;
use mina_tree::TokenId;

use super::{
    context::Context,
    types::{self, Account, DaemonStatus, PublicKey},
};

pub struct Query;

#[Object]
impl Query {
    async fn ping(&self) -> &'static str {
        "pong"
    }

    async fn daemon_status(&self) -> DaemonStatus {
        DaemonStatus {
            chain_id: "1337".to_string(),
        }
    }

    async fn sync_status(&self) -> &'static str {
        "SYNCED"
    }

    async fn account<'ctx>(
        &self,
        ctx: &async_graphql::Context<'ctx>,
        token: Option<types::TokenId>,
        public_key: PublicKey,
    ) -> Result<Account> {
        let Context {
            da_layer_client: _,
            ledger,
        } = ctx.data::<Context>().expect("Context should be set");

        let token_id = TokenId::from(
            token
                .unwrap_or(types::TokenId("1".into()))
                .0
                .parse::<u64>()?,
        );

        let public_key = CompressedPubKey::from_address(&public_key.0).map_err(|e| {
            async_graphql::Error::new(format!("Error parsing public key: {}", e.to_string()))
        })?;

        let account = ledger
            .lock()
            .await
            .get_or_create_account(public_key, Some(token_id))
            .map_err(|e| {
                async_graphql::Error::new(format!(
                    "Error getting or creating account: {}",
                    e.to_string()
                ))
            })?;

        Ok(Account::from(account))
    }
}
