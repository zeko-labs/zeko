# Zeko sequencer

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

Archive node is used for mina blockchain to index the history of the blockchain. You can optionally run the archive alongside the node's daemon, which dispatches new blocks to the archive. In zeko rollup the blockcreator is the sequencer, and since currently it's not possible to run multiple sequencers, you need to run the client that subscribes to the sequencer and dispatches the new blocks to the archive.

To use standard mina archive node to index the history of zeko rollup, you need to run the zeko archive relay adapter, that can subscribe to zeko sequencer for new changes and relay them to the archive node.
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
           --commitment-period <int> \
           --rollback-checker-interval <int> \
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

## Transfers

To transfer funds from L1 to L2 and vice versa, you need to send a zkapp command to the bridge contract which dispatches an action representing the transfer.
To create such zkapp command you need the proved account update which you can get from the sequencer.

The sequencer exposes async api for creating a transfer request for proving and then for fetching the proved account update.

### Create transfer request

The returned key can be used to poll the sequencer for the proved account update.

```graphql
mutation {
  proveTransfer(input: {
    "address": "recipient base58 address",
    "amount": "uint64 amount to transfer",
    "direction": DEPOSIT | WITHDRAW
  }) {
    key
  }
}
```

### Fetch proved account update

The returned json can be used to create a zkapp command for the bridge contract.

```graphql
query {
  transfer(key: "key returned from proveTransfer")
}
```

### Full example

```typescript
import { AccountUpdate, Mina } from "o1js";

// Send request to prove transfer
const response = await fetch(sequencerUrl, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    query: `
        mutation ($transferInput: TransferInput!) {
          proveTransfer(input: $transferInput) {
            accountUpdateKey
          }
        }
      `,
    variables: {
      transferInput: {
        address: sender.toPublicKey().toBase58(),
        amount: amount,
        direction: "WRAP",
      },
    },
  }),
});

const key = (await response.json()).data.proveTransfer.accountUpdateKey;

// Poll for proved account update
let transferCallForest: string | null = null;
while (true) {
  console.log("Polling for transferJson");
  const response = await fetch(sequencerUrl, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      query: `
          query ($key: String!) {
            transfer(key: $key)
          }
        `,
      variables: { key },
    }),
  });

  transferCallForest = (await response.json()).data.transfer;

  if (transferCallForest !== null) {
    break;
  } else {
    await new Promise((resolve) => setTimeout(resolve, 3_000));
  }
}

// Create zkapp command
let txn = await Mina.transaction(sender.toPublicKey(), () => {
  AccountUpdate.fundNewAccount(sender.toPublicKey());

  const fundsUpdate = AccountUpdate.createSigned(sender.toPublicKey());
  fundsUpdate.balance.subInPlace(amount);
});

// Append proved account update to the command
JSON.parse(transferCallForest).forEach((accountUpdate) => {
  txn.transaction.accountUpdates.push(AccountUpdate.fromJSON(accountUpdate));
});

await txn.sign([sender]).send();
```

### Fetching pending/completed transfers

All of the transfer requests both ways (deposits/withdrawals) are submitted as actions.
Both inner and outer smart contract handling transfers hold action states to keep track of the pending/completed transfers.

The layout of state is following:

Outer account (deployed on L1):

- 0 - `ledger_hash`: rollup ledger hash
- 1 - `all_withdrawals`: withdrawals action state that has been updated with last commit
- 2 - `withdrawals_processed`: withdrawals that has been processed so far
- action state: all deposits action state.

Inner account (deployed on L2):

- 0 - `all_deposits`: deposits action state that has been updated right before last commit by sequencer
- 1 - `deposits_processed`: deposits that has been processed so far
- action state: all withdrawals action state.

You can fetch any interval of actions through graphql api or o1js tooling from both layers.

#### Fetching actions from graphql

```graphql
query {
  actions(
    input: {
      address: "address of the smart contract"
      fromActionState: "action state to start from"
      endActionState: "action state to end with"
    }
  ) {
    actionData {
      data
    }
  }
}
```

Here are some common intervals to fetch:

- Processed deposits: `[initialActionState, inner.state[1]]`
- Processed withdrawals: `[initialActionState, outer.state[2]]`
- Deposits in process: `[inner.state[1], inner.state[0]]`
- Withdrawals in process: `[outer.state[2], outer.state[1]]`
- Pending deposits: `[inner.state[0], outer.actionState]`
- Pending withdrawals: `[outer.state[1], inner.actionState]`
