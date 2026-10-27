# Zeko

[Zeko](https://twitter.com/ZekoLabs) is a zero-knowledge rollup settling on Ethereum.
It supports recursive proofs and smart contracts built with o1js, with ETH and
ERC-20 bridging between Ethereum and Zeko. The Zeko rollup can settle natively
to Ethereum as well as Mina.

This repository implements Zeko's ledger, transaction and bridge circuits,
sequencer, and provers. The separate Ethereum settlement pipeline lives in
[ethereum-settlement](https://github.com/zeko-labs/ethereum-settlement), where
SP1 verifies Zeko's Pickles proofs for verification on Ethereum.

## Security Audits

This repository has component-specific security assessments by
[zkSecurity](https://zksecurity.xyz/). The reports cover the reviewed Zeko
code and revisions described in [`audits/`](./audits/), not the entire
`/zeko` repository and not the wider `zeko-labs` GitHub organization.

- [Audit index](./audits/)
- [2025 circuits audit](./audits/zksecurity-2025-circuits/)
- [2026 circuits, sequencer, DA, deployment, and recovery audit](./audits/zksecurity-2026-circuits-sequencer-da/)

This repository is a fork of the [Mina codebase](https://github.com/MinaProtocol/mina).
Mina-derived types, cryptography, and transaction formats remain part of the
implementation; they do not imply that the current rollup settles on Mina.
The Zeko-related code is in the [`zeko`](./src/app/zeko) directory.

## Licensing

Zeko-authored and repository-maintained code is available under the Apache
License, Version 2.0. The canonical license text is in [LICENSE](./LICENSE),
with a copy in [LICENSES/APACHE-2.0.txt](./LICENSES/APACHE-2.0.txt). The
Zeko-specific protocol code is covered by
[src/app/zeko/LICENSE.md](./src/app/zeko/LICENSE.md).

This repository is based on Mina and may include upstream or third-party
components with their own license notices. Those notices remain applicable to
the files or components they identify. See [LICENSING.md](./LICENSING.md) for
the repository licensing scope.
