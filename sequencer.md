# Mina zk rollup sequencer

## Overview

The Mina zk rollup sequencer is a component of the Mina zk rollup that is responsible for block producing. There may be one sequencer or many using a consensus protocol. For v1, there is just one sequencer.

The sequencer has following responsibilities:

- Accept user transactions through the GraphQL API with the same interface as the Mina daemon.
- Keep the rollup's state with the full ledger
- Observe L1 rollup contract for deposit events, relay them to the DA layer and update the local deposit requests queue.
- Observe DA layer for new deposit requests and add them to the local deposit requests queue.
- Consolidate deposit requests and user transactions into a block.
- Submit the block to the DA layer.
- Submit the block as a state transition to the Mina daemon to store the state.

## 1. GraphQL API

For the rollup to be compatible with the Mina L1 tooling, the GraphQL API of the sequencer is the same as the Mina daemon.
The difference is that the rollup won't be accepting stake delegation transactions.

This is the rough schema that the sequencer will be implementing:

```graphql
mutation {
    sendPayment(
        signature: SignatureInput
        input: SendPaymentInput!
    ): SendPaymentPayload!

    sendZkapp(input: SendZkappInput!): SendZkappPayload!

    addNewBlock(
        block: BlockInput!
    ): AddNewBlockPayload!

    setBestChain(
        stateHash: String!
        height: Int!
    ): SetBestChainPayload!
}

query {
    syncStatus: SyncStatus! # always returns synced
    account(
        token: TokenId
        publicKey: PublicKey!
    ): Account
    accounts(publicKey: PublicKey!): [Account!]!
    tokenOwner(tokenId: TokenId!): Account
    tokenAccounts(tokenId: TokenId!): [Account!]!
    block(
        height: Int
        stateHash: String
    ): Block!
    genesisBlock: Block!
    transactionStatus(
        zkappTransaction: ID
        payment: ID
    ): TransactionStatus!
    validatePayment(
        signature: SignatureInput
        input: SendPaymentInput!
    ): Boolean!
}
```

## 2. State

The sequencer keeps the full ledger state.
Since we don't want to be reimplementing the state structuring and fetching logic, the sequencer will hold the state in the Mina daemon.
We add new mutation to the graphql API of mina daemon accepting new blocks that will be added to the transition frontier and mutation to set the currently best tip for rollbacks.

Using the `--demo-mode` option the daemon will be in synced mode by default and won't be participating in the consensus.

## 3. Deposit requests

The sequencer needs to know current pending requests and their status.

The `included` status is indicated by present receipt on DA layer with the valid block hash in the current rollup state. In the case of a rollback, the receipt stays on the DA layer, therefore there might be present receipts for the same deposit request in multiple rollup states.

After the rollback the deposit request can rollback to the `pending` state meaning that the transaction is still `included` on L1, or to the `non-existent` state meaning that the transaction is no longer `included` on L1.

Therefore after the L1 rollback, the sequencer needs to rebuild the deposit requests queue from the current rollup state, validating the deposit request transaction against the L1 state and validate the deposit request receipts against the rollup state.

## 4. Block production

The sequencer will be producing blocks in the following way:

1. Observe the L1 rollup contract for deposit events, relay them to the DA layer and update the local deposit requests queue.
2. Observe DA layer for new deposit requests and add them to the local deposit requests queue.
3. Consolidate deposit requests and user transactions into a block.
4. Submit the block to the DA layer.
5. Wait for the DA signoff of the block.
6. Submit the state to the L1 rollup contract.
7. Submit the block as a state transition to the Mina daemon to store the state.
