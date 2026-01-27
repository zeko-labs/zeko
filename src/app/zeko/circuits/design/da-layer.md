# DA layer (multisig) design

This document describes the offchain data availability (DA) layer as used by
Zeko. The DA layer is a committee of nodes that collectively attest to the
availability of the ledger diffs that the sequencer applies.

The core idea: each DA node only signs a ledger hash if it can reconstruct that
ledger by applying a posted diff to a known source ledger hash. Signatures are
collected by the sequencer and verified on L1 during normal commits.

## Data model

```ocaml
type ledger_hash

type diff =
  { source_ledger_hash : ledger_hash
  ; target_ledger_hash : ledger_hash
  ; changed_accounts : (int * Account.t) list
  ; command_with_action_step_flags : (User_command.t * bool list) option
  ; time : int64 option
  }
```

Notes:

- `changed_accounts` is a sparse, index-addressed update list.
- `command_with_action_step_flags` used for archival purposes, optional
- `time` added by da node to indicate when the diff arrived

## Node API (conceptual)

```ocaml
val post_diff : ledger_openings:Sparse_ledger.t -> diff -> Signature.t
val get_diff : ledger_hash -> diff option
```

The production implementation uses Async.Rpc endpoints; the conceptual API
matches the node semantics.

## Node validation rules

The core idea: provide sparse ledger that is already reconstructible, with openings to all the changed accounts.
We provide those openings so that da node doesn't have to keep the whole ledger up to date in memory for every possible state.

Given a `diff` and a `ledger_openings` witness for the source ledger:

1. Check `ledger_openings.root = diff.source_ledger_hash`.
2. Check `diff.source_ledger_hash` is known to the DB (or equals genesis).
3. Check all indices in `changed_accounts` are unique.
4. Apply each `(index, account)` to the sparse ledger openings to produce a
   new ledger root, and assert it equals `diff.target_ledger_hash`.
5. If `command_with_action_step_flags` is present, verify that updating receipt
   chain hashes for the command matches the target ledger's receipt chain.
6. Sign `diff.target_ledger_hash`.
7. Store the diff under `diff.target_ledger_hash`.

The receipt-chain check binds the diff to the transaction commitment history;
it does not prove authorization (since transaction commitment excludes
signatures), but it prevents signing a ledger hash that does not match the
command's receipt-chain updates.

## Sequencer-side client

The sequencer posts diffs to all DA nodes and collects their signatures for a
quorum. A strict ordering is required: a DA node will refuse a diff whose source
ledger hash does not match its current known head. The sequencer therefore uses
an ordered enqueueing client (`Da_layer.Client.Sequencer`) to serialize posting.

At commit time, the sequencer requests signatures for the target ledger hash
and includes the multisig witness in the L1 commit.

## Availability and recovery

DA nodes persist diffs keyed by `target_ledger_hash`. A new sequencer (or a
restarted sequencer) can reconstruct ledger history by replaying diffs from DA
nodes in order, starting from genesis or a known checkpoint.

## Trust model

- A DA node only signs if it can locally reconstruct the target ledger hash.
- The L1 circuit only checks the multisig signature over the target ledger
  hash, so the availability and integrity of the diff data is guaranteed by
  the DA committee's signing policy.
- Emergency mode (actions-based DA) bypasses the multisig by proving ledger
  transitions directly in-circuit from per-account diffs.
