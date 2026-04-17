# Zeko Explorer JetStream Message Contract

This document is the Zeko-owned contract for explorer-facing NATS/JetStream
publishing from the sequencer and backfill service.

## Stream

- Stream name: `ZEKO_L2`
- Subjects:
  - `zeko.l2.transactions`
  - `zeko.l2.finality`
  - `zeko.health`
- Duplicate window: 120 seconds
- Storage: file
- Retention: limits

Sequencer startup best-effort ensures the stream exists with these subjects when
`--nats-url` is configured. If NATS is not configured, sequencer publishing is a
no-op. If NATS is configured but unavailable, the sequencer logs a warning and
explorer publishing remains disabled rather than crashing.

The standalone backfill service is stricter: `--nats-url` is required, startup
fails if it cannot connect to JetStream, and dropped backfill publishes fail the
backfill job.

## Transaction Diffs

Subject: `zeko.l2.transactions`

Headers:

```text
Nats-Msg-Id: <target_ledger_hash_decimal>
```

`target_ledger_hash_decimal` is `Ledger_hash.to_decimal_string
target_ledger_hash`. Publishing the same transaction diff with the same target
ledger hash must deduplicate at the JetStream stream level.

Payload:

```json
{
  "kind": "user_command | fee_transfer | sync_replay | genesis_replay",
  "target_ledger_hash": "<Ledger_hash.to_yojson target_ledger_hash>",
  "genesis": false,
  "diff": {
    "...": "Da_layer.Diff.Stable.V3.to_yojson diff"
  }
}
```

Fields:

- `kind`: why this diff is being published.
- `target_ledger_hash`: target ledger hash encoded with
  `Ledger_hash.to_yojson`.
- `genesis`: `true` when replaying the first diff from the genesis source hash.
- `diff`: DA-layer diff encoded with `Da_layer.Diff.Stable.V3.to_yojson`.

## Finality

Subject: `zeko.l2.finality`

Headers:

```text
Nats-Msg-Id: finality-<status>-<target_ledger_hash_decimal>
```

`status` is the payload status string, and `target_ledger_hash_decimal` is
`Ledger_hash.to_decimal_string target_ledger_hash`. This keeps `proved` and
`committed` finality messages independently deduplicated for the same target
ledger hash.

Payload:

```json
{
  "status": "proved | committed",
  "source_ledger_hash": "<Ledger_hash.to_yojson source_ledger_hash>",
  "target_ledger_hash": "<Ledger_hash.to_yojson target_ledger_hash>",
  "timestamp": "<Block_time.to_yojson timestamp>"
}
```

Fields:

- `status`: finality state for the target ledger hash.
- `source_ledger_hash`: source ledger hash encoded with
  `Ledger_hash.to_yojson`.
- `target_ledger_hash`: target ledger hash encoded with
  `Ledger_hash.to_yojson`.
- `timestamp`: publisher timestamp encoded with `Block_time.to_yojson`.

## Health

Subject: `zeko.health`

Headers: none required.

Payload:

```json
{
  "service": "sequencer-nats-publisher",
  "instance_id": "<publisher instance id>",
  "status": "ok",
  "last_published_hash": "<Ledger_hash.to_yojson hash>",
  "unproved_hash": "<Ledger_hash.to_yojson hash>",
  "timestamp": "<Block_time.to_yojson timestamp>"
}
```

Fields:

- `service`: publishing service identity.
- `instance_id`: publisher instance identity.
- `status`: publisher health/status string.
- `last_published_hash`: optional last published ledger hash.
- `unproved_hash`: optional unproved ledger hash.
- `timestamp`: publisher timestamp encoded with `Block_time.to_yojson`.
