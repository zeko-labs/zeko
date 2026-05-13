# Zeko Explorer JetStream Message Contract

This document is the Zeko-owned contract for explorer-facing NATS/JetStream
publishing from the sequencer and backfill service.

## Streams

- Stream name: `zeko-l2`
  - Subjects: `zeko.l2.>`
  - Duplicate window: 120 seconds
  - Max age: 90 days
  - Storage: file
  - Retention: limits
- Stream name: `zeko-health`
  - Subjects: `zeko.health`
  - Max messages: 1000
  - Storage: file
  - Retention: limits

Sequencer startup best-effort ensures the streams exist with these subjects when
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
  "source_ledger_hash": "<Ledger_hash.to_yojson source_ledger_hash>",
  "target_ledger_hash": "<Ledger_hash.to_yojson target_ledger_hash>",
  "timestamp": "<Block_time.to_yojson timestamp>",
  "acc_set": "<Field.to_yojson acc_set>",
  "command": {
    "type": "signed_command | zkapp_command",
    "raw": "<User_command.to_yojson command>",
    "action_step_flags": [true]
  },
  "changed_accounts": [
    {
      "index": 0,
      "account": "<Account.to_yojson account>"
    }
  ],
  "command_with_action_step_flags": "<raw DA diff command field>",
  "genesis": false,
  "diff": {
    "...": "Da_layer.Diff.Stable.V3.to_yojson diff"
  }
}
```

Fields:

- `kind`: why this diff is being published.
- `source_ledger_hash`: source ledger hash encoded with
  `Ledger_hash.to_yojson`.
- `target_ledger_hash`: target ledger hash encoded with
  `Ledger_hash.to_yojson`.
- `timestamp`: diff timestamp encoded with `Block_time.to_yojson`.
- `acc_set`: account-set root from the DA diff.
- `command`: normalized command envelope. `null` for fee transfers and diffs
  without a command.
- `changed_accounts`: post-state accounts affected by the diff, normalized as
  `{index, account}` objects.
- `command_with_action_step_flags`: raw DA-layer command field, retained for
  consumers that need the exact OCaml-derived encoding.
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
  "level": "proved | committed",
  "ledger_hash": "<Ledger_hash.to_yojson target_ledger_hash>",
  "source_ledger_hash": "<Ledger_hash.to_yojson source_ledger_hash>",
  "target_ledger_hash": "<Ledger_hash.to_yojson target_ledger_hash>",
  "timestamp": "<Block_time.to_yojson timestamp>"
}
```

Fields:

- `level`: finality state for the target ledger hash.
- `ledger_hash`: target ledger hash encoded with `Ledger_hash.to_yojson`.
- `status`: alias of `level`, retained for compatibility with existing
  Zeko-side consumers.
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
