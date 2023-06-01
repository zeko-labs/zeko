use tokio::sync::Mutex;

use crate::{da_layer::DALayer, ledger::Ledger};

pub struct Context {
    #[allow(dead_code)]
    pub da_layer_client: DALayer,
    pub ledger: Mutex<Ledger>,
}
