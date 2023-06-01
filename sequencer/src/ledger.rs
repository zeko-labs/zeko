use ark_ff::Zero;
use mina_hasher::Fp;
use mina_signer::CompressedPubKey;
use mina_tree::{
    scan_state::{
        currency::{Amount, Fee, Length, Magnitude, Slot},
        scan_state::ConstraintConstants,
        transaction_logic::{
            apply_transactions,
            protocol_state::{EpochData, EpochLedger, ProtocolStateView},
            signed_command::SignedCommand,
            transaction_applied::TransactionApplied,
            zkapp_command::ZkAppCommand,
            Transaction, UserCommand,
        },
    },
    staged_ledger::sparse_ledger::LedgerIntf,
    Account, AccountId, Mask, TokenId,
};

pub struct Ledger {
    pub tree: Mask,

    pub commited_transactions: Vec<Transaction>,
    pub staged_transactions: Vec<Transaction>,

    pub contraint_constants: ConstraintConstants,
    pub protocol_state_view: ProtocolStateView,
}

impl Ledger {
    pub fn new(depth: usize, account_creation_fee: u64, genesis_accounts: Vec<Account>) -> Self {
        let mut tree = Mask::create(depth);

        genesis_accounts.into_iter().for_each(|account| {
            tree.create_new_account(account.id(), account).unwrap();
        });

        // TODO: Figure out how to set these values
        Self {
            tree,
            commited_transactions: vec![],
            staged_transactions: vec![],
            contraint_constants: ConstraintConstants {
                sub_windows_per_window: 0,
                ledger_depth: depth as u64,
                work_delay: 0,
                block_window_duration_ms: 0,
                transaction_capacity_log_2: 0,
                pending_coinbase_depth: 0,
                coinbase_amount: Amount::from_u64(0),
                supercharged_coinbase_factor: 0,
                account_creation_fee: Fee::from_u64(account_creation_fee),
                fork: None,
            },
            protocol_state_view: ProtocolStateView {
                snarked_ledger_hash: Fp::zero(),
                blockchain_length: Length::from_u32(0),
                min_window_density: Length::from_u32(0),
                last_vrf_output: (),
                total_currency: Amount::from_u64(0),
                global_slot_since_genesis: Slot::zero(),
                staking_epoch_data: EpochData {
                    ledger: EpochLedger {
                        hash: Fp::from(0),
                        total_currency: Amount::from_u64(0),
                    },
                    seed: Fp::zero(),
                    start_checkpoint: Fp::zero(),
                    lock_checkpoint: Fp::zero(),
                    epoch_length: Length::from_u32(1),
                },
                next_epoch_data: EpochData {
                    ledger: EpochLedger {
                        hash: Fp::from(0),
                        total_currency: Amount::from_u64(0),
                    },
                    seed: Fp::zero(),
                    start_checkpoint: Fp::zero(),
                    lock_checkpoint: Fp::zero(),
                    epoch_length: Length::from_u32(1),
                },
            },
        }
    }

    pub fn get_or_create_account(
        &mut self,
        pub_key: CompressedPubKey,
        token_id: Option<TokenId>,
    ) -> Result<Account, Box<dyn std::error::Error>> {
        let account_id: AccountId =
            AccountId::create(pub_key, token_id.unwrap_or(TokenId::default()));

        let (_, account, _) = self.tree.get_or_create(&account_id)?;

        Ok(account)
    }

    pub fn apply_payment(
        &mut self,
        signed_command: SignedCommand,
    ) -> Result<TransactionApplied, Box<dyn std::error::Error>> {
        let receipts = apply_transactions(
            &self.contraint_constants,
            Slot::from_u32(1),
            &self.protocol_state_view,
            &mut self.tree,
            [Transaction::Command(UserCommand::SignedCommand(Box::new(
                signed_command.clone(),
            )))]
            .as_ref(),
        )?;

        self.staged_transactions
            .push(Transaction::Command(UserCommand::SignedCommand(Box::new(
                signed_command,
            ))));

        match receipts.first() {
            Some(receipt) => Ok(receipt.clone()),
            None => Err("No receipts".into()),
        }
    }

    pub fn apply_zkapp_command(
        &mut self,
        zkapp_command: ZkAppCommand,
    ) -> Result<TransactionApplied, Box<dyn std::error::Error>> {
        let receipts = apply_transactions(
            &self.contraint_constants,
            Slot::from_u32(1),
            &self.protocol_state_view,
            &mut self.tree,
            [Transaction::Command(UserCommand::ZkAppCommand(Box::new(
                zkapp_command.clone(),
            )))]
            .as_ref(),
        )?;

        self.staged_transactions
            .push(Transaction::Command(UserCommand::ZkAppCommand(Box::new(
                zkapp_command,
            ))));

        match receipts.first() {
            Some(receipt) => Ok(receipt.clone()),
            None => Err("No receipts".into()),
        }
    }
}
