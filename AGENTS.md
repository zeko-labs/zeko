# AGENTS.md

## Repo overview

- This is a fork of the Mina blockchain repository for the L2 project Zeko.
- Zeko-specific code lives under `src/app/zeko`.
- Changes to upstream Mina code are either prefixed with `zeko_` or marked by a `ZEKO NOTE` comment.

## Key directories

- `src/app/zeko/circuits`: zero-knowledge circuits for the rollup smart contract.
- `src/app/zeko/circuits/design`: rollup and bridge design docs/specs (see notes below).
- `src/app/zeko/da_layer`: multisig DA layer code.
- `src/app/zeko/sequencer`: off-chain service that accepts transactions, proves them, sends to DA nodes, collects signatures, then submits to L1.
- `src/app/zeko/sequencer/prover`: proving service used by the sequencer.

## Build

- Build the Zeko project with:
  - `dune build ./src/app/zeko/sequencer ./src/app/zeko/da_layer ./src/app/zeko/signer`
- The build can take longer than 10 seconds; prefer running with a longer timeout (e.g., 120s) in CI or local scripts.

## Testing

- Build the Zeko project first
- run ./src/app/zeko/tests/run-sequencer-test.sh fake 3 true false

## Project language

- OCaml (Dune build system).

## Design docs (circuits)

- Primary, current spec: `src/app/zeko/circuits/design/rollup-centralized-spec.md`.
- `src/app/zeko/circuits/design/README.md` notes that other files may be out of date; treat `src/app/zeko/circuits/design/old` as historical context only.
- Core rollup model: outer (L1) and inner (L2) accounts with action states; commits synchronize a prefix of the outer action state into the inner account; inner steps append to outer action state and advance its length.
- Commit semantics: sequencer supplies a transaction SNARK and a `valid_while` range; commit action records ledger hash, inner action state, synchronized outer action state, and length; `valid_while` range must be bounded.
- Rollup state includes `sequencer`, `pause_key`, `da_key`, and `acc_set`; pause is an emergency switch and can be updated via governance.
- Transaction logic differences: failed transactions are not processed; time preconditions exist (currently unimplemented) and commits must use compatible slot ranges.
- Bridge/token transfer (see `bridge-explanation.md`, `bridge-spec.md`): deposits are witnessed on the rollup outer account and finalized on L2 if followed by a commit before timeout; withdrawals are witnessed on L2 and finalized on L1 after a delay; helper accounts track last processed indices to prevent double spends; cancellations are handled via timeout and helper indices.
- Bridge governance: may be separate from rollup governance; circuits can enforce vk hash matching across inner/outer bridge accounts; a failsafe design rotates multiple outer bank accounts to limit impact of a circuit bug.

## Circuits (code concepts)

- Action states are typed fields with optional length tracking (`rollup_state.ml`); inner app state stores the outer action state+length, outer app state stores the ledger hash, inner action state+length, sequencer key, pause key, DA multisig commitment, and account-set root.
- Outer actions are either `Commit` (rollup step summary) or `Witness` (arbitrary action payload); inner actions are `Witness` only (`rollup_state.ml`).
- `rule_commit.ml` verifies the transaction SNARK, DA multisig signatures over the target ledger hash, slot-range bounds, and action-state-extension proofs; it updates outer app state, emits a `Commit` action, and requires a sequencer signature as a child update.
- `rule_commit.ml` has an emergency branch that verifies `Verify_emergency_folders` (Verify_both_ases + Count_commits), enforces a `max_sequencer_inactivity` gap, and allows commit without the sequencer precondition if no commits occurred since the last one.
- `rule_action_witness.ml` posts a `Witness` action on L1 and forbids actions while paused; `rule_pause.ml` lets the pause key sign an update that sets `paused = true`.
- `rule_inner_sync.ml` advances the inner account’s stored outer action state using an A.S.E. proof; `rule_inner_action_witness.ml` posts inner `Witness` actions.
- `txn_rules.ml` builds the rollup transaction SNARK with branches for signed commands, zkapp commands (proved/unproved), and merge; the statement type is `Txn_state.Zeko_stmt`.
- `txn_state.ml` defines the rollup SNARK statement, including source/target ledger hashes, account-set roots, slot ranges, and local zkapp state (must be empty at commit).
- `ase.ml` implements action-state-extension folding circuits (with/without length) and is used by commit and inner sync to prove action-state progression.
- `account_set.ml`/`indexed_merkle_tree.ml` implement the indexed merkle tree used for the account-set root recorded in the outer state.
- `multisig.ml` defines DA-layer multisig checking; only the commitment hash is stored in the outer state (`da_key`).
- Bridge circuits use `bridge_state.ml` for enable/disable window parameters and helper-user indices; `bridge_rules.ml` wires deposit/withdrawal/cancel/enable/disable rules for Mina and custom tokens.
- `folder.ml`/`folder.mli` implement a generic recursive “state machine” folding circuit (leaf/extend/merge rules) used by action-state extension proofs; it folds arrays of elements with optional mid-proof selection and configurable iteration counts.

## DA layer (code concepts)

- Each DA node stores and signs diffs that deterministically transform a source ledger hash into a target ledger hash; signatures attest the node can reconstruct the target ledger.
- `diff.ml` defines the DA diff format: source ledger hash, changed accounts (index + account), optional command with action-step flags, and a timestamp (V2 adds time; V1 is time-less).
- `core.ml` validates a posted diff by checking source hash, DB presence/genesis, unique indices, and recomputed target hash; it also validates receipt-chain updates for included commands before signing the target hash and persisting the diff.
- `rpc.ml` exposes Async.Rpc endpoints for posting diffs, fetching diffs, checking presence, listing keys, and fetching signatures/ledger-hash chains.
- `node.ml` runs the server with a local KV DB, signs ledger hashes, supports syncing from other nodes, and can return the signature for a target hash.
- `client.ml` provides RPC helpers with retry logic and (for sequencer usage) enforces ordered posting of diffs; it also includes optional relational DB tables for cached diffs/signatures.
- `db.ml` is the KV storage for diffs plus an index and migration marker; `migrations.ml` handles version upgrades (e.g., adding the top-level version tag).

## Sequencer (core flow)

- `zeko_sequencer.ml` is the main orchestrator: it validates incoming user commands, applies them to the local ledger/IMT, queues proofs, posts DA diffs, and periodically commits to L1.
- Transaction application uses `zeko_transaction_logic.ml`, which applies Mina signed payments and zkapp commands to the local ledger, updates the indexed merkle tree, and returns `Txn_snark_witness` segments plus a sparse source ledger for proof inputs.
- Command validation checks pool capacity, minimum fee (dynamic based on prover queue size), slot-range bounds, and signature/proof validity via `verifier.ml` before applying to state.
- Applied zkapp commands have events/actions persisted to the archive; fee accumulation is tracked in local KV state and periodically converted into a fee-transfer command.
- DA integration: after applying a command, the sequencer builds a `Da_layer.Diff` from changed accounts and enqueues it to all DA nodes in order; commits use DA multisig signatures over the target ledger hash.
- Proof generation uses a parallel merger (`Parallel_merger` via `zeko_sequencer.ml`) with three stages: `Base` creates per-transaction snarks, `Merge` combines two snarks into a larger proof, and `Commit` finalizes a batch into an L1 zkapp command.
- `committer.ml` assembles the outer commit proof: it proves the txn snark, validates A.S.E. proofs, includes DA multisig witness, and constructs the rollup commit account update; it also persists commit witnesses for manual resend.
- `update_inner_account` (in `zeko_sequencer.ml`) syncs L1 actions into the L2 inner account using an inner-sync proof, then applies a dummy-fee zkapp command locally to advance action state before committing.
- `prover/lib/prover.ml` is the prover worker: it handles txn-snark branches (signed/zkapp/merge), folder/A.S.E. proofs, inner sync, and outer commit proofs; `prover/lib/client.ml` sends jobs via a message queue and caches A.S.E. proofs in Postgres.
- `executor.ml` is the L1 sender: it signs zkapp commands, infers/refreshes nonce, retries on transient failures, and serializes submissions to avoid nonce races.

## Circuits (engineering knowhow)

- Zkapp action payloads are limited to ~100 field elements; large data should be compressed into hashes or split across transactions (emergency DA uses a single account per tx).
- Action payload encoding is done via `zeko_util.var_to_actions`, which pushes a struct’s field elements into actions as data-as-hash.
- Action-state extension proofs (A.S.E.) are built with `ase.ml` + `folder.ml`; these are the canonical way to prove action-state progression with bounded iterations.
- Emergency commits rely on a counted-commit fold: `Count_commits` produces `{source_action_state; target_action_state; n_commits}` and the emergency rule requires `n_commits = 0` and that counting starts right after the last commit action.
- Valid-while ranges are inclusive in Mina; commit rules enforce max window size and subset checks against transaction snark slot ranges.

## Mina zkApp model (transaction logic)

- zkApp transactions are a call forest of account updates; execution walks the forest with a call stack, deriving `caller_id` based on `may_use_token` and parent relationships.
- Each account update checks: account/ledger inclusion, account preconditions, protocol-state preconditions, valid-while range, and authorization (proof or signature) against the transaction commitment.
- Transaction commitments are computed from the account-update call forest; `use_full_commitment` switches between transaction and full commitments for authorization and replay protection.
- Permissions gate every field update (balance, app state, action state, permissions, delegate, nonce, voting-for, zkapp URI, verification key); updates are applied only when the corresponding permission controller authorizes.
- Action state update logic pushes events into action slots and may shift slots; in Zeko this shift can be forced by the sequencer (no time-based shifting).
- Receipt chain hashes are updated for account updates authorized by proof/signature, using the full transaction commitment and the account-update index.
- Account creation fees are handled inside zkapp logic; they may be deducted from balance or from fee excess depending on flags.
- Failed zkapp updates are disallowed on Zeko: local_state.success must hold for the last account update; failed transactions are rejected rather than partially applied.

## Transaction SNARK structure

- Transaction snarks support base proofs for signed commands and zkapp command segments, plus merge proofs that combine two proofs into one.
- Zkapp command segments are categorized by authorization pattern: `Opt_signed`, `Opt_signed_opt_signed`, or `Proved`; proved segments require a side-loaded verification key.
- The zkapp snark uses implied merkle roots from account+path to validate ledger inclusion and ensures consistent commitments across segments.
