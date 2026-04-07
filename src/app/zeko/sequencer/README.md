# Zeko sequencer

## Zeko's Transaction Powerhouse

Think of the sequencer as the conductor of an orchestra in Zeko. It plays a vital role in keeping everything running smoothly. Here's what it does:

1. **Transaction Collector**: The sequencer acts like a tireless collector, gathering these transactions and applying them to the current state.
2. **Proof Verifier**: The sequencer prooves the validity of these transactions using zero-knowledge proofs. This ensures only legitimate transactions enter the system.
3. **Batch Processor**: The sequencer doesn't process transactions one by one. Instead, it efficiently groups them into batches for more efficient settling on layer 1.
4. **Layer 1 Bridge**: Once a batch is ready, the sequencer sends it to the main chain (Layer 1) via a smart contract. This keeps Layer 1 informed about the activity happening on Zeko.

## Build

The repo-managed OCaml setup imports `opam.export` and then runs
`./scripts/pin-external-packages.sh`, which pins `nats-client` and
`nats-client-async` from GitHub into the switch. No `nats-ml` submodule checkout
is required.

```bash
DUNE_PROFILE=devnet dune build ./src/app/zeko/sequencer
```

## Tests

```bash
dune build
./src/app/zeko/sequencer/tests/run-sequencer-test.sh {fake | real} <num_provers>
```

## Run

Running the sequencer exposes the Graphql API on the port `-p`. The Graphql schema is a subset of the L1 Graphql API joined with the L1 Graphql API for fetching of actions/events.

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export DUNE_PROFILE=devnet
dune exec ./run.exe -- \
    -p <int?> \
    --l1-uri <string> \
    --archive-uri <string> \
    --commitment-period <float?> \
    --max-pool-size <int?> \
    --da-node <string list> \
    --da-keys <string> \
    --da-quorum <int> \
    --mq-host <string> \
    --db-dir <string?> \
    --checkpoints-dir <string?> \
    --postgres-uri <string> \
    --deposit-delay-blocks <int?> \
    --fee-modifier <float?> \
    --minimum-fee <float?> \
    --nats-url <string?> \
    --slot-acceptance <float?> \
    --commit-validity-period <int?>
```

`--nats-url` enables explorer event publishing. Without it, the sequencer keeps
the existing behavior and all NATS publish paths become no-ops.

When NATS is enabled, the sequencer emits:

- `zeko.l2.transactions`
- `zeko.l2.finality`
- `zeko.health`

`zeko.l2.transactions` carries:

- `kind`: `user_command`, `fee_transfer`, `sync_replay`, or `genesis_replay`
- `target_ledger_hash`
- `genesis`
- `diff`

`diff` uses the existing `Da_layer.Diff.Stable.V3` JSON shape and every
transaction message includes `Nats-Msg-Id: <target_ledger_hash>`.

`zeko.l2.finality` carries:

- `status`: `proved` or `committed`
- `source_ledger_hash`
- `target_ledger_hash`
- `timestamp`

`zeko.health` carries:

- `component`
- `instance_id`
- `status`
- `timestamp`

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Explorer backfill service

The backfill service republishes historical DA diffs into the same
`zeko.l2.transactions` NATS subject used by the live sequencer path.

Build it with:

```bash
DUNE_PROFILE=devnet dune build ./src/app/zeko/sequencer/explorer
```

Run it with:

```bash
export DUNE_PROFILE=devnet
dune exec ./explorer/run.exe -- \
    -p <int?> \
    --da-node <string list> \
    --nats-url <string>
```

The GraphQL HTTP endpoint stays on `/graphql`.
The service requires `--nats-url` because each backfill republishes historical
diffs into NATS.

Available GraphQL operations:

- `backfill(fromHash, toHash) -> BackfillJob`
- `backfillJob(id) -> BackfillJob | null`
- `health -> Health`

The GraphQL-SSE endpoint is `/graphql/stream`.

Supported subscription:

- `backfillProgress(id) -> BackfillProgress`

Use the same GraphQL subscription document against `/graphql/stream`; the
service executes the schema subscription and streams `backfillProgress`
responses as GraphQL-SSE events.

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

## Deploy rollup contract to L1

The following script deploys the rollup contract on the L1 with the initial state, which is the genesis ledger of the rollup.

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export DUNE_PROFILE=devnet
dune exec ./deploy.exe -- \
    --l1-uri <string> \
    --ledger-input <string?> \
    --faucet-account <string?> \
    --da-node <string list> \
    --pause-key <string> \
    --sequencer-key <string> \
    --da-keys <string> \
    --da-quorum <int> \
    --account-creation-fee <string>
```

Run help to see the options:

```bash
dune exec ./deploy.exe -- --help
```

## Using archive node as indexer

Archive node is used for mina blockchain to index the history of the blockchain. You can optionally run the archive alongside the node's daemon, which dispatches new blocks to the archive. In Zeko rollup the blockcreator is the sequencer, and since currently it's not possible to run multiple sequencers, you need to run the client that subscribes to the sequencer and dispatches the new blocks to the archive.

To use standard mina archive node to index the history of Zeko rollup, you need to run the Zeko archive relay adapter, that can subscribe to Zeko sequencer for new changes and relay them to the archive node.
You can run the adapter with the following command:

```bash
export DUNE_PROFILE=devnet
dune exec ./archive_relay/run.exe -- \
    --zeko-uri <string> \
    --archive-host <string> \
    --archive-port <int> \
    --bootstrap
```

To run the adapter from docker see the section below.

### Running archive relay adapter from docker

```bash
docker run --entrypoint archive_relay \
           dcspark/zeko
           --zeko-uri <string> \
           --archive-host <string> \
           --archive-port <int> \
           --bootstrap
```

## Manual commit

In case automatic commit transaction fails and sequencer gets in the unrecoverable state, `cli.exe` can be used to manually resend commit transactions.

To list all available commit transactions:

```bash
export DUNE_PROFILE=devnet
dune exec ./cli.exe -- committer list
```

To get json of the commit transaction:

```bash
export DUNE_PROFILE=devnet
dune exec ./cli.exe -- committer get --source <source-ledger-hash> --target <target-ledger-hash>
```

To resend the commit transaction:

```bash
export DUNE_PROFILE=devnet
export MINA_PRIVATE_KEY="base58 signer private key"
dune exec ./cli.exe -- committer send
    --source <source-ledger-hash> \
    --target <target-ledger-hash> \
    --l1-uri <l1-uri> \
    --fee <fee> \
    --nonce <optional-nonce>
```
