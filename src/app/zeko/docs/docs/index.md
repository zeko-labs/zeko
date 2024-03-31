---
sidebar_label: "Introduction"
sidebar_position: 1
---


# Zeko

Zeko is a framework for creating zk-rollups specifically designed for zero-knowledge applications (zkApps) that support both privacy and *recursive SNARKs* (the ability to combine two proofs A→B, B→C to a single constant-sized proof A→C).

Zeko constructs these layer 2s by inheriting the circuits from [Mina Protocol](https://minaprotocol.com/) as its base allowing it to both act as an isomorphic (akin to a [type-2 rollup](https://vitalik.eth.limo/general/2022/08/04/zkevm.html)) for Mina and as a way to [scale other L1s like Ethereum](https://blog.paimastudios.com/paima-zk-layer).
