# Explorer Indexing Plan

## High Level

This work adds a first-party NATS publishing path to the Zeko sequencer and a standalone backfill service in this repo.

When it is done, this repo will be able to:

- publish L2 transaction events as the sequencer processes diffs
- publish L2 finality events as batches move from proved to committed
- publish health heartbeats for freshness monitoring
- republish historical diff ranges into NATS through a standalone backfill service

This matters because explorer and indexing systems need a direct, replayable event stream from the sequencer and DA layer, without depending on downstream reconstruction of sequencer state.

## 1. Integrate `nats-ml` into this repo

Work:

- Bring `[nats-ml](https://github.com/Hebilicious/nats-ml)` into this repo from GitHub.
- Wire it into the repo build so sequencer code can depend on it.
- Make it available to both the sequencer and the backfill service.

Result:

- NATS publishing depends on a repo-local integration of `nats-ml`, not an external manual install step.

## 2. Add sequencer NATS configuration

Target files:

- `src/app/zeko/sequencer/run.ml`
- `src/app/zeko/sequencer/lib/zeko_sequencer.ml`
- `src/app/zeko/sequencer/dune`

Work:

- Add `--nats-url` to the sequencer CLI.
- Extend sequencer config and runtime state with an optional NATS client.
- Create the `nats-ml` client at startup when `--nats-url` is present.
- Keep all publishing paths as no-ops when NATS is not configured.

Result:

- The sequencer runs unchanged without NATS.
- The sequencer publishes to NATS when configured.

## 3. Add one shared NATS event module

Target files:

- `src/app/zeko/sequencer/lib/`

Work:

- Add a small module that owns NATS subjects, headers, and JSON encoding.
- Use this module from both live sequencer publishing and the backfill service.
- Keep the event contract in one place.

Subjects:

- `zeko.l2.transactions`
- `zeko.l2.finality`
- `zeko.health`

Headers:

- `Nats-Msg-Id: <target_ledger_hash>` on every `zeko.l2.transactions` message

### `zeko.l2.transactions` schema

Fields:

- `kind`
  - `user_command`
  - `fee_transfer`
  - `sync_replay`
  - `genesis_replay`
- `target_ledger_hash`
- `genesis`
- `diff`

`diff` fields:

- `source_ledger_hash`
- `changed_accounts`
- `command_with_action_step_flags`
- `timestamp`
- `acc_set`

Encoding rules:

- `diff` uses the existing JSON shape derived from `Da_layer.Diff.Stable.V3`.
- `target_ledger_hash` is the post-diff ledger hash passed to `Da_layer.Client.enqueue_diff`.
- `genesis` matches the flag passed to `Da_layer.Client.enqueue_diff`.

### `zeko.l2.finality` schema

Fields:

- `status`
  - `proved`
  - `committed`
- `source_ledger_hash`
- `target_ledger_hash`
- `timestamp`

Encoding rules:

- `proved` is emitted from the successful committer result.
- `committed` is emitted after `State.Last_committed_ledger.set`.

### `zeko.health` schema

Fields:

- `service`
- `instance_id`
- `status`
- `last_published_hash`
- `unproved_hash`
- `timestamp`

Encoding rules:

- `service = "sequencer-nats-publisher"`
- `status = "ok"`

Result:

- This repo has one stable event boundary for explorer ingestion.

## 4. Publish L2 transaction events from the sequencer

Target file:

- `src/app/zeko/sequencer/lib/zeko_sequencer.ml`

Integration points:

- `apply_user_command`
- `apply_fee_transfer`
- `sync`

Work:

- After `Da_layer.Client.enqueue_diff` in `apply_user_command`, publish one `zeko.l2.transactions` message with `kind = user_command`.
- After `Da_layer.Client.enqueue_diff` in `apply_fee_transfer`, publish one `zeko.l2.transactions` message with `kind = fee_transfer`.
- During `sync`, publish replayed diffs after they are re-enqueued.
- Use `kind = genesis_replay` when the replayed diff was enqueued with `genesis = true`.
- Use `kind = sync_replay` for the rest of the replayed diffs.
- Set `Nats-Msg-Id` from `target_ledger_hash`.

Result:

- Live traffic and replayed traffic produce the same transaction event format.

## 5. Publish finality events from the sequencer

Target file:

- `src/app/zeko/sequencer/lib/zeko_sequencer.ml`

Integration points:

- `run_committer`
- `Merger.Commit.process`

Work:

- Publish `zeko.l2.finality` with `status = proved` when `run_committer` receives `Some (stmt, _)`.
- Publish `zeko.l2.finality` with `status = committed` after `State.Last_committed_ledger.set` in the commit path.
- Include both source and target ledger hashes in both cases.

Result:

- The explorer stack can track proved and committed progress directly from NATS.

## 6. Publish sequencer health heartbeats

Target file:

- `src/app/zeko/sequencer/lib/zeko_sequencer.ml`

Work:

- Add a periodic heartbeat loop.
- Emit `zeko.health` messages using the shared event module.
- Generate one sequencer instance id at startup and reuse it for all heartbeats from that process.

Result:

- The sequencer emits a freshness signal even when no transactions are being processed.

## 7. Add the backfill service

Target area:

- new standalone executable under `src/app/zeko/sequencer/`
- build wiring in `src/app/zeko/sequencer/dune`

Work:

- Add a standalone OCaml service that reads diffs from the DA layer and republishes them to NATS.
- Use `nats-ml` for publishing.
- Reuse the shared NATS event module so backfilled messages match live messages exactly.
- Track backfill jobs in memory.
- Generate one service instance id at startup.

GraphQL API:

- `backfill(fromHash, toHash) -> BackfillJob`
- `backfillJob(id) -> BackfillJob`
- `health -> Health`

GraphQL-SSE subscription:

- `backfillProgress(id) -> BackfillProgress`

`BackfillJob` fields:

- `id`
- `fromHash`
- `toHash`
- `status`
- `diffsPublished`
- `error`
- `createdAt`
- `startedAt`
- `finishedAt`

`BackfillProgress` fields:

- `id`
- `status`
- `diffsPublished`
- `error`

`Health` fields:

- `ok`
- `instanceId`
- `startedAt`

Transport:

- Use GraphQL over HTTP for query and mutation.
- Use GraphQL-SSE for `backfillProgress`.

Publishing rules:

- Republish to `zeko.l2.transactions`.
- Use the same payload schema and `Nats-Msg-Id` rule as the live sequencer path.
- Emit `kind = genesis_replay` only when replaying a genesis diff.
- Emit `kind = sync_replay` for all other backfilled diffs.

Result:

- Missing L2 ranges can be republished into NATS without changing sequencer state.

## 8. Update sequencer docs

Target file:

- `src/app/zeko/sequencer/README.md`

Work:

- Document `--nats-url`.
- Document the three emitted subjects.
- Document the exact payload families.
- Document how to run the backfill service.
- Document the backfill GraphQL and GraphQL-SSE surface.

Result:

- The repo docs describe the explorer-facing ingestion path end to end.

## 9. Add tests as first-class work

Work:

- Add tests for the shared event module:
  - transaction payload encoding
  - finality payload encoding
  - health payload encoding
  - `Nats-Msg-Id` generation
- Add sequencer publishing tests for:
  - `apply_user_command`
  - `apply_fee_transfer`
  - `sync` replay
  - `run_committer` proved event
  - commit-path committed event
  - health heartbeat emission
- Add backfill service tests for:
  - `backfill` job creation
  - ordered republishing across a hash range
  - progress updates over GraphQL-SSE
  - `health` response contents
  - matching payload shape between live and backfilled messages

Result:

- The explorer-facing boundary is covered directly in this repo.

## Delivery Order

1. Integrate `nats-ml` into this repo.
2. Add sequencer startup/config wiring for NATS.
3. Add the shared NATS event module.
4. Publish `zeko.l2.transactions`.
5. Publish `zeko.l2.finality`.
6. Publish `zeko.health`.
7. Add the backfill service with GraphQL and GraphQL-SSE.
8. Finish docs and tests.

## Deliverable

After this work, the `zeko` repo provides:

- repo-local `nats-ml` integration
- optional sequencer publishing to NATS
- replay of historical diffs during sync
- finality and health events
- a standalone backfill service with GraphQL and GraphQL-SSE
- test coverage around the explorer-facing publish boundary
