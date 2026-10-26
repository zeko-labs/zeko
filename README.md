# Zeko

[Zeko](https://twitter.com/ZekoLabs) is a fully isomorphic L2 for the Mina protocol ecosystem.
We support recursion in smart contracts natively. The Zeko rollup can settle natively to Ethereum as well as Mina. The separate Ethereum settlement pipeline is located in the [ethereum-settlement repository](https://github.com/zeko-labs/ethereum-settlement).

## Security Audits

This repository has component-specific security assessments by [zkSecurity](https://zksecurity.xyz/). The reports cover the reviewed Zeko code and revisions described in [`audits/`](./audits/), not the entire `/zeko` repository and not the wider `zeko-labs` GitHub organization.

- [Audit index](./audits/)
- [2025 circuits audit](./audits/zksecurity-2025-circuits/)
- [2026 circuits, sequencer, DA, deployment, and recovery audit](./audits/zksecurity-2026-circuits-sequencer-da/)

This repository is a fork of the [Mina codebase](https://github.com/MinaProtocol/mina).
The Zeko related code is in the [`zeko`](./src/app/zeko) directory.

The license for code under the `zeko` subdirectory is [custom](./src/app/zeko/LICENSE.md)
though it will be soon updated to the same Apache license at the root of the repository.
