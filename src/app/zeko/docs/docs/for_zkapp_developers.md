# For zkapp developers

This guide will walk you through the process of using a custom zeko network to write zkapps. If this is your first time using Mina, we recommend you start with the official [Mina documentation](https://docs.minaprotocol.com/).

Zeko is mostly isomorpic to Mina, so you can use the same tools and libraries to develop zkapps as you would use to develop zkapps on Mina. There are only few places where you need to be specific about the network you are using. It is the same as the difference between using mainnet and testnet.

## Auro wallet custom network

For the zkapp to be sending transactions to the zeko network, you need to add and set the network in the Auro wallet.

Following API calls are available in the Auro wallet to manipulate with the network:

- getting current network: [mina_requestnetwork](https://docs.aurowallet.com/general/reference/api-reference/methods/mina_requestnetwork)
- adding network: [mina_addchain](https://docs.aurowallet.com/general/reference/api-reference/methods/mina_addchain)
- switching network: [mina_switchchain](https://docs.aurowallet.com/general/reference/api-reference/methods/mina_switchchain)

## O1js custom network

To use O1js with custom network you need to specify it when specifying the Network.

TODO: add example once O1js implements this
