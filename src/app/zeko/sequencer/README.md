# Zeko sequencer

## Zeko's Transaction Powerhouse

Think of the sequencer as the conductor of an orchestra in Zeko. It plays a vital role in keeping everything running smoothly. Here's what it does:

1. **Transaction Collector**: The sequencer acts like a tireless collector, gathering these transactions and applying them to the current state.
2. **Proof Verifier**: The sequencer prooves the validity of these transactions using zero-knowledge proofs. This ensures only legitimate transactions enter the system.
3. **Batch Processor**: The sequencer doesn't process transactions one by one. Instead, it efficiently groups them into batches for more efficient settling on layer 1.
4. **Layer 1 Bridge**: Once a batch is ready, the sequencer sends it to the main chain (Layer 1) via a smart contract. This keeps Layer 1 informed about the activity happening on Zeko.

## Build

```bash
DUNE_PROFILE=devnet dune build
```

## Tests

```bash
# Run local network to imitate L1
DUNE_PROFILE=devnet dune exec ./tests/local_network/run.exe -- --db-dir l1_db

DUNE_PROFILE=devnet dune runtest
```

## Run

Running the sequencer exposes the Graphql API on the port `-p`. The Graphql schema is a subset of the L1 Graphql API joined with the L1 Graphql API for fetching of actions/events.

```bash
export DA_PROVIDER="da evm provider"
export DA_PRIVATE_KEY="da layer private key"
export MINA_PRIVATE_KEY="base58 signer private key"
export DUNE_PROFILE=devnet
dune exec ./run.exe -- \
    -p <int?> \
    --rest-server <string> \
    --zkapp-pk <string> \
    --max-pool-size <int?> \
    --commitment-period <float?> \
    --da-contract-address <string?> \
    --db-dir <string?> \
    --test-accounts-path <string?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Deploy rollup contract to L1

The following script deploys the rollup contract on the L1 with the initial state, which is the genesis ledger of the rollup.

```bash
export MINA_PRIVATE_KEY="base58 signer private key"
export DUNE_PROFILE=devnet
dune exec ./deploy.exe -- \
    --rest-server <string>
    --test-accounts-path <string?>
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

## Use with docker

Build:

```bash
make docker
```

Run:

```bash
docker run -p <port>:<port> \
           -v <local-db-path>:<container-db-path> \
           -e DA_PROVIDER=<da-evm-provider> \
           -e DA_PRIVATE_KEY=<da-private-key> \
           -e MINA_PRIVATE_KEY=<mina-private-key> \
           dcspark/zeko -p <port> \
           --zkapp-pk <zkapp-pk> \
           --rest-server <mina-node-graphql> \
           --archive-uri <mina-archive-node-graphql> \
           --commitment-period <int> \
           --da-contract-address <da-layer-contract> \
           --db-dir <container-db-path>
```

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

In case automatic commit transaction fails and sequencer gets in the unrecoverable state, `committer.exe` can be used to manually resend commit transactions.

To list all available commit transactions:

```bash
export DUNE_PROFILE=devnet
dune exec ./committer.exe -- list
```

To get json of the commit transaction:

```bash
export DUNE_PROFILE=devnet
dune exec ./committer.exe -- get --source <source-ledger-hash> --target <target-ledger-hash>
```

To resend the commit transaction:

```bash
export DUNE_PROFILE=devnet
export MINA_PRIVATE_KEY="base58 signer private key"
dune exec ./committer.exe -- send
    --source <source-ledger-hash> \
    --target <target-ledger-hash> \
    --l1-uri <l1-uri> \
    --fee <fee> \
    --nonce <optional-nonce>
```
