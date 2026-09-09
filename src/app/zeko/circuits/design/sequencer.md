# Sequencer design

This document describes the sequencer's offchain responsibilities and how it
coordinates transaction application, proving, DA posting, and L1 commits.

## Overview

The sequencer is the single offchain orchestrator that:

- Exposes a GraphQL API for transaction submission.
- Applies user commands to the local ledger and indexed merkle tree (IMT) account set.
- Posts account diffs to the DA layer and collects signatures.
- Proves transaction SNARKs and merges them via the parallel merger.
- Periodically produces L1 commit transactions.
- Can recover from restarts by syncing from DA history.

## Lifecycle of a transaction

### 1. Intake and validation

- Command arrives via GraphQL API.
- The sequencer validates:
  - pool capacity and minimum fee (dynamic)
  - signatures / proofs (via verifier)
  - valid-while slot range constraints
- Invalid or failed commands are rejected (Zeko does not process failed
  transactions).

## Slot-range handling

Each user command can include a valid-while slot range. The sequencer accepts
transactions whose slot ranges are sufficiently far in the future, as measured
by the `slot_acceptance` threshold. This ensures that there is enough time to
batch, prove, and commit the transaction before its slot precondition expires.

When a batch is committed, the commit uses the merged slot precondition across
all transactions in that batch. As a result, the acceptance window is a safety
buffer: transactions with tighter ranges would make the commit likely to fail
or waste proving work. If the sequencer fails to commit within that window,
the proving work for those transactions is wasted and the batch must be
reconstructed with valid preconditions.

### 2. Apply to local state

`Zeko_transaction_logic.apply_user_command_unchecked` updates:

- the local ledger database
- the indexed merkle tree (account set)

It returns:

- a `Txn_snark_witness`, the witness for proving the transaction SNARK

### 3. Post to DA layer

From the applied command, the sequencer constructs a DA `diff`:

- `(index, account)` changes
- optional command with action-step flags for archive
- source/target ledger hashes

The diff is enqueued to DA nodes in strict order using
`Da_layer.Client.Sequencer`. The sequencer later requests signatures for the
current target ledger hash to satisfy the DA multisig requirement on commit.

### 4. Queue proving job

The transaction witness is added to the parallel merger as a `Base` job. The
merger immediately starts proving and merging in the background. This produces
merged proofs ready for commit.

### 5. Periodic commit

At each commitment period:

- The sequencer may run `update_inner_account` to synchronize the inner account
  with L1 actions (if there are new actions).
- It requests the latest merged proof from the parallel merger.
- It constructs the commit witness, including:
  - transaction SNARK proof and statement
  - action-state extension proofs (outer/inner)
  - DA multisig signatures
  - slot-range constraints
- It proves the outer commit circuit and sends the resulting zkapp command to L1.

When the Ethereum gateway is enabled, the sequencer waits for the previous
settlement to reach finality before synchronizing outer actions or preparing
the next state-bound settlement. Commit-only inner sync and settlement
submission share a gate, so neither can observe or change the outer action
state while the other is in flight. Finality polling happens before the commit
enters the transaction-application queue, allowing user commands to continue
entering while the sequencer waits. These ordering guarantees are covered by
[`settlement_finality_test.ml`](../../sequencer/tests/settlement_finality_test.ml).

If there is nothing to commit (e.g., only an inner sync happened), the commit
step is skipped.

## Recoverability and syncing from DA

On startup, the sequencer attempts to reconstruct local state:

1. It checks for existing ledger/IMT databases.
2. If missing, it fetches current onchain
   rollup state to determine the last committed ledger hash.
   - it accounts also for transaction pool to prevent conflicts
3. It calls `sync`, which:
   - fetches the committed ledger hash from L1 (or commit in transaction pool)
   - replays diffs from DA nodes in order
   - applies each diff to the local ledger and IMT
   - re-enqueues diffs locally (so the DA client has history to sync new da nodes)
   - applies events/actions for zkapp commands
4. It verifies that the reconstructed ledger hash matches the onchain committed
   hash.

This allows a new sequencer (or a restarted one) to resynchronize from DA
history without requiring access to the previous sequencer's local state.

## Commit recovery

The sequencer persists commit witnesses so that failed L1 submissions can be
retried manually via `cli.exe committer`. This avoids a stuck state if an L1
transaction fails to be sent or accepted.
