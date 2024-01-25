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
DUNE_PROFILE=devnet dune exec ./run.exe -- \
    -p <int?> \
    --rest-server <string> \
    --zkapp-pk <string> \
    --signer <string> \
    --max-pool-size <int?> \
    --commitment-period <float?> \
    --da-contract-address <string?> \
    --db-dir <string?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Deploy rollup contract to L1

The following script deploys the rollup contract on the L1 with the initial state, which is the genesis ledger of the rollup.

```bash
DUNE_PROFILE=devnet dune exec ./deploy.exe -- \
    --rest-server <string> \
    --signer <string>
```

Run help to see the options:

```bash
dune exec ./deploy.exe -- --help
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

TODO
