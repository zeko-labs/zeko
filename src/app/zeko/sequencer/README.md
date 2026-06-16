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
    --slot-acceptance <float?> \
    --commit-validity-period <int?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Signer service auth and TLS

The signer service listens only on localhost and requires a shared auth token
for all RPC calls. Set the same `ZEKO_SIGNER_AUTH_TOKEN` in the signer process
and every process that connects to it, such as the sequencer or DA node.

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"

dune exec ../signer/cli.exe -- run \
    --port 9000 \
    --allow-zkapp-signing \
    --max-fee 10 \
    --max-balance-change 1000000
```

Then run clients with the same token:

```bash
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"
dune exec ./run.exe -- --signer localhost:9000 ...
```

TLS is optional but recommended whenever signer traffic may cross a container,
VM, or host boundary. Start the signer with a certificate and key:

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export ZEKO_SIGNER_AUTH_TOKEN="long random signer token"

dune exec ../signer/cli.exe -- run \
    --port 9000 \
    --allow-zkapp-signing \
    --max-fee 10 \
    --max-balance-change 1000000 \
    --tls-cert-file /path/to/signer-cert.pem \
    --tls-key-file /path/to/signer-key.pem
```

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
