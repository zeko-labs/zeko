# Zeko

[Zeko](https://twitter.com/ZekoLabs) is a zero-knowledge rollup settling on Ethereum.
It supports recursive proofs and smart contracts built with o1js, with ETH and
ERC-20 bridging between Ethereum and Zeko.

This repository implements Zeko's ledger, transaction and bridge circuits,
sequencer, and provers. The experimental Ethereum settlement integration lives
in [ethereum-settlement](https://github.com/zeko-labs/ethereum-settlement), where
SP1 verifies Zeko's Pickles proofs for verification on Ethereum.

This repository is a fork of the [Mina codebase](https://github.com/MinaProtocol/mina).
Mina-derived types, cryptography, and transaction formats remain part of the
implementation; they do not imply that the current rollup settles on Mina.
The Zeko related code is in the [`zeko`](./src/app/zeko) directory.

The license for code under the `zeko` subdirectory is [custom](./src/app/zeko/LICENSE.md)
though it will be soon updated to the same Apache license at the root of the repository.
