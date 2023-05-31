# Mina zk rollup sequencer

## Overview

The Mina zk rollup sequencer is a component of the Mina zk rollup that is responsible for block producing. There may be one sequencer or many using a consensus protocol. For v1, there is just one sequencer.

The sequencer has following responsibilities:

- Accept user transactions through the GraphQL API with the same interface as the Mina daemon.
- Apply user transactions to the ledger.
- Periodically consolidate user transactions into a batch.
- Submit the batch to the DA layer.
- Wait for the DA signoff of the batch.
- Submit the state to the L1 rollup contract.
- Listen for rollbacks on L1

## Table of contents

- [Mina zk rollup sequencer](#mina-zk-rollup-sequencer)
  - [Overview](#overview)
  - [Table of contents](#table-of-contents)
  - [1. GraphQL API](#1-graphql-api)
  - [2. Ledger](#2-ledger)
  - [2.1. Rollback optimalization](#21-rollback-optimalization)
  - [3. Batch consolidation](#3-batch-consolidation)
  - [4. Data Availability layer](#4-data-availability-layer)
  - [5. Bootstrap](#5-bootstrap)

## 1. GraphQL API

For the rollup to be compatible with the Mina L1 tooling, the GraphQL API of the sequencer will be almost same as the Mina daemon GraphQL API.
The L2 rollup don't have concept of blocks and delegation, therefore the GraphQL API will be simplified.

```graphql
mutation {
    sendPayment(
        signature: SignatureInput
        input: SendPaymentInput!
    ): SendPaymentPayload!

    sendZkapp(input: SendZkappInput!): SendZkappPayload!
}

query {
    syncStatus: SyncStatus! # always `synced`

    account(
        token: TokenId
        publicKey: PublicKey!
    ): Account
    accounts(publicKey: PublicKey!): [Account!]!

    tokenOwner(tokenId: TokenId!): Account
    tokenAccounts(tokenId: TokenId!): [Account!]!

    transactionStatus(
        zkappTransaction: ID
        payment: ID
    ): TransactionStatus!

    validatePayment(
        signature: SignatureInput
        input: SendPaymentInput!
    ): Boolean!

    batch(
        id: BatchId!
        transactions: [TransactionId!]!
    ): Batch
}
```

## 2. Ledger

The sequencer needs to hold the state of the whole ledger.
The sequencer will use ledger exported from snarkyjs.
Since the snarkyjs is a npm package, to avoid embedding the ledger in the sequencer, the ledger will be standalone service written in typescript.

Ledger service will expose following rest endpoints for the ledger operations:

```
GET /account/:publicKey

POST /applyZkappCommand
POST /applyUserCommand

POST /applyBatches

POST /applyGenesis
```

`/applyGenesis` is used in case of bootstrapping and erases the current ledger. `/applyBatches` is the endpoint used for faster boostrapping.

Currently exported ledger in snarkyjs exposes only [`applyJsonTransaction`](https://github.com/o1-labs/snarkyjs-bindings/blob/54da2c27228984075bf0cacd54c468d7317cb258/ocaml/lib/snarky_js_bindings_lib.ml#LL2773C13-L2773C13) which is wrapped [`apply_zkapp_command`](https://github.com/o1-labs/snarkyjs-bindings/blob/54da2c27228984075bf0cacd54c468d7317cb258/ocaml/lib/snarky_js_bindings_lib.ml#LL2527C5-L2527C36). For basic payments we need to expose also [`apply_user_command`](https://github.com/MinaProtocol/mina/blob/develop/src/lib/transaction_logic/mina_transaction_logic.ml#L1094).

## 2.1. Rollback optimalization

The sequencer can cache `k` last states to avoid full boostrap.

Following endpoints will be exposed:

```
GET /availableStates
POST /rollbackTo/:batchId
```

To store multiple states, deep copy of ledger has to be implemented.

## 3. Batch consolidation

The sequencer will periodically consolidate user transactions into a batch.
The batch will be a list of zkapp transactions and payments.

## 4. Data Availability layer

Data availability layer is an evm chain with multisignature smart contract and set of validators.
Validators are represented by mina public keys and their purpose is to sign off the batch, confirming that the batch is available.

The sequencer will submit the batch to the DA layer. Batch consists of list of zkapp command and payments.
For the batch to be signable with schnorr signature, it needs to be encoded into field elements.
The sequencer needs to be able decode the batch from field elements.

The solidity struct of the batch is following:

`bytes32` is a field element.

```solidity
struct Batch {
  bytes32 id; // Poseidon hash of zkapp commands, payments and previous batch
  bytes32 previousBatch;
  bytes32[] zkappCommands;
  bytes32[] payments;
}
```

Every batch is unique by id, therefore validators can sign off the batch by signing the id of the batch.

## 5. Bootstrap

The sequencer needs to be able to bootstrap the chain from the genesis ledger state.
The single source of truth for current state is L1 contract and for batches data is DA layer.

The bootstrap process will be following:

1. Fetch the current `batchId` from L1 contract.
2. Fetch available historical states from ledger service.
3. If the `batchId` is present in available states, use the state as current state.
4. If not, fetch the batches in retrospective order from DA layer starting from current `batchId` until the genesis state.
5. Apply the batches to the ledger.

The bootstrap process will be triggered on startup and on L1 rollbacks.
