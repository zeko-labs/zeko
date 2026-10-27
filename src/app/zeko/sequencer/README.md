# Zeko sequencer

## Zeko's Transaction Powerhouse

Think of the sequencer as the conductor of an orchestra in Zeko. It plays a vital role in keeping everything running smoothly. Here's what it does:

1. **Transaction Collector**: The sequencer acts like a tireless collector, gathering these transactions and applying them to the current state.
2. **Proof Verifier**: The sequencer prooves the validity of these transactions using zero-knowledge proofs. This ensures only legitimate transactions enter the system.
3. **Batch Processor**: The sequencer doesn't process transactions one by one. Instead, it efficiently groups them into batches for more efficient settling on layer 1.
4. **Layer 1 Bridge**: Once a batch is ready, the sequencer sends it to the main chain (Layer 1) via a smart contract. This keeps Layer 1 informed about the activity happening on Zeko.

## Build

```bash
DUNE_PROFILE=devnet dune build ./src/app/zeko/sequencer
```

## Tests

```bash
dune build -j 1 ./src/app/zeko/sequencer/tests/settlement_finality_test.exe \
  ./src/app/zeko/sequencer/tests/settlement_durability_test.exe
env -u ZEKO_ETHEREUM_GATEWAY_TOKEN ZEKO_CIRCUITS_CONFIG=test ZEKO_CIRCUITS_MODE=fake \
  ./_build/default/src/app/zeko/sequencer/tests/settlement_finality_test.exe
env -u ZEKO_ETHEREUM_GATEWAY_TOKEN ZEKO_CIRCUITS_CONFIG=test ZEKO_CIRCUITS_MODE=fake \
  ZEKO_TEST_POSTGRES_PORT=5433 \
  ./_build/default/src/app/zeko/sequencer/tests/settlement_durability_test.exe
```

Both targets link the fake proving implementation. The durability test requires
an isolated PostgreSQL instance with `postgres:postgres` credentials; it creates
and drops only its `settlement_durability_test` database. It checks transaction
rollback during witness handoff and restores the saved submission and successor
witnesses through a fresh connection pool. It needs no signer, RabbitMQ, DA node,
or proving worker.

The full fake integration runner requires Docker and the compiled sequencer,
signer, DA, and fake-prover binaries. Fake mode uses `cli_fake.exe` for key
generation, `prover/cli_fake.exe` for proving, and `sequencer_test_fake.exe` for
the test process:

```bash
./src/app/zeko/sequencer/tests/run-sequencer-test.sh fake 1 true true
```

Its four arguments are mode, prover count, log redirection, and service-readiness
waiting. Use one fake prover on a constrained development machine. Real proving
must be requested separately and run on a suitable machine.

For a persistent Nix development container named `zeko-dev`, start isolated test
sidecars from the host. Sharing its network namespace makes their ports available
on the runner's `localhost` without giving the development container Docker
access. These limits leave room for one fake prover on a 16 GB machine:

```bash
docker run -d --name zeko-test-postgres \
  --network container:zeko-dev --memory 512m --cpus 1 \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid \
  -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres \
  postgres:16-alpine postgres -p 5433
docker run -d --name zeko-test-rabbitmq \
  --network container:zeko-dev --memory 768m --cpus 1 \
  -e RABBITMQ_DEFAULT_USER=guest -e RABBITMQ_DEFAULT_PASS=guest \
  -e 'RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS=+S 1:1' \
  rabbitmq:4-management
```

Inside the development container, run from the core repository in its Nix shell
with `curl`, `jq`, and `nc` available:

```bash
ZEKO_TEST_EXTERNAL_SERVICES=true \
  ./src/app/zeko/sequencer/tests/run-sequencer-test.sh fake 1 true true
```

This mode checks PostgreSQL on port 5433 and RabbitMQ on port 5672, and waits for
prover consumers through RabbitMQ's management API. It defaults to
`http://localhost:15672` and the isolated test credentials `guest:guest`. Override
these with `ZEKO_TEST_RABBITMQ_MANAGEMENT_URL`,
`ZEKO_TEST_RABBITMQ_MANAGEMENT_USER`, and
`ZEKO_TEST_RABBITMQ_MANAGEMENT_PASSWORD` when needed. The runner still stops its
own signer, DA, L1, and prover processes; externally managed sidecars remain until
the host removes them:

```bash
docker rm -f zeko-test-postgres zeko-test-rabbitmq
```

## Export Ethereum deployment artifacts

Ethereum deployment uses a dedicated exporter instead of deriving release
artifacts from a sequencer test scenario. The exporter starts isolated local
L1, database, queue, signer, DA, and prover services, creates the production
genesis ledger, and makes one real sequencer commit. It writes exactly one
settlement artifact together with `genesis-ledger.json` and a
`deployment-manifest.json` that binds the sequencer identity and DA topology.

Set `ZEKO_CIRCUITS_CONFIG`, `ZEKO_DEPLOY_CONFIG`,
`ZEKO_DEPLOYMENT_SEQUENCER_PRIVATE_KEY`, and one
`ZEKO_DEPLOYMENT_DA<N>_PRIVATE_KEY` per selected DA node, then run:

```bash
mkdir -p build/deployment-artifacts
src/app/zeko/sequencer/run-deployment-export.sh \
  build/deployment-artifacts 3 2
```

The final two arguments are the DA node count and signature quorum. The runner
supports one through three local DA nodes. This performs real local Pickles
proving but does not request or generate an SP1 proof. It selects an available
local port range automatically; set `ZEKO_DEPLOYMENT_EXPORT_PORT_OFFSET` only
when a specific range is required.

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
    --slot-acceptance <float?> \
    --slot-duration <int?> \
    --commit-validity-period <int?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Ethereum settlement recovery

When `ZEKO_ETHEREUM_GATEWAY_TOKEN` is set, the sequencer uses the gateway's
settlement reservation and outcome APIs in addition to its Mina-compatible
GraphQL endpoint. The same token authenticates those REST requests through
`X-API-Key`. The gateway and sequencer must both implement this protocol.

Before inner-account synchronization or outer proof preparation, the sequencer
reserves a finalized outer-state checkpoint. Its durable owner ID lives in the
ledger KV database. Preparing reservations expire after 120 seconds and are
renewed every 30 seconds; a gateway-accepted job retains ownership until the
gateway resolves its outcome. A busy bridge writer delays preparation outside
the transaction admission queue. A lost or stale reservation cannot authorize a
new submission.

The sequencer stores the source/target ledger witness before submitting a commit.
The PostgreSQL transaction that saves it also removes that commit's original
base witnesses, leaving subsequent transaction witnesses available for restart.
Migration 6 adds `settlement_attempt`, which records the signed command, its
gateway hash, and the complete immutable submission payload before the HTTP
request. Keep this table, the existing `commit` table, and the ledger/IMT together
when backing up or restoring a sequencer. Existing commit-witness JSON remains
compatible; no history rewrite is required.

Acceptance is distinct from finality. Recovery checks both the durable gateway
outcome and the finalized ledger root. It completes a saved A-to-B commit before
allowing a queued B-to-C commit. Unknown submission outcomes replay the exact
stored payload; a definite fenced rejection or retryable terminal job can cause
the outer proof to be rebuilt against a fresh reservation. Gateway-owned retries
for RPC failures or insufficient funds remain the same job. Recovery never
silently rolls back the accepted ledger pointer, skips an unknown root, or
extends an expired transaction proof's validity interval.

After local ledger initialization, HTTP and GraphQL remain available while
settlement recovery is running:

- `GET /healthz` returns 200 while the process serves requests.
- `GET /readyz` returns 200 when admission is ready and 503 while recovery blocks
  admission. Its body contains the settlement phase.
- GraphQL fields `settlementStatus`, `settlementReady`, `settlementError`, and
  `settlementFinalizedLedgerHash` expose the recovery state and last verified
  finalized ledger. Phases include `initializing`, `ready`, `settling`,
  `waiting_for_outer_writer`, `recovering`, and `blocked`.

Ordinary admission remains available during healthy pending settlement and
writer waits. Startup reconciliation, failed-job recovery, and consistency
errors reject new transactions with a retryable availability error. An unknown
finalized root, non-retryable rejection, or expired transaction proof requires
operator investigation. Inspect the saved job and witness instead of deleting
the ledger, resetting the database, or repeatedly restarting the process.

For rollout, stop the old sequencer before enabling mandatory gateway
reservations, inspect/drain existing gateway jobs without discarding ambiguous
Ethereum submissions, deploy the gateway migration and API, then start the new
sequencer with its existing state. Verify `/healthz`, `/readyz`, and the finalized
ledger before routing new writes. An old sequencer cannot submit to a gateway
that requires reservations. Use liveness to detect a dead process and readiness
to route writes; an unfunded relayer or a blocked recovery is not a reason to
erase state or run a hard reset.

## Signer service auth and TLS

The signer service listens on localhost by default and requires a shared auth
token for all RPC calls. Set the same `ZEKO_SIGNER_AUTH_TOKEN` in the signer
process and every process that connects to it, such as the sequencer or DA
node.

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"

dune exec ../signer/cli.exe -- run \
    --port 9000 \
    --allow-zkapp-signing \
    --max-fee 0.00025 \
    --max-balance-change 1000000
```

Then run clients with the same token:

```bash
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"
dune exec ./run.exe -- --signer localhost:9000 ...
```

Use `--host` when the signer must accept connections from another container,
VM, or host. Non-loopback binds require TLS:

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"

dune exec ../signer/cli.exe -- run \
    --host 0.0.0.0 \
    --port 9000 \
    --allow-zkapp-signing \
    --max-fee 0.00025 \
    --max-balance-change 1000000 \
    --tls-cert-file /path/to/signer-cert.pem \
    --tls-key-file /path/to/signer-key.pem
```

When using Docker, expose the signer only on a dedicated private container
network and connect to it using its container DNS name. Do not publish its port
on a public host interface. For trusted private networks where TLS is
terminated elsewhere, `--allow-insecure-remote-binding` explicitly permits a
non-loopback bind without TLS.

Clients enable TLS by setting the trusted CA/certificate file. The expected
hostname defaults to the host in `--signer`; set `ZEKO_SIGNER_TLS_HOSTNAME` when
the certificate name differs from the connection host.

```bash
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"
export ZEKO_SIGNER_TLS_CA_FILE="/path/to/ca-or-signer-cert.pem"
export ZEKO_SIGNER_TLS_HOSTNAME="localhost"

dune exec ./run.exe -- --signer localhost:9000 ...
```

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
