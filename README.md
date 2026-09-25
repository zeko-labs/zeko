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

## Licensing And Commercial Terms

Original Mina-derived code remains under the Apache License, Version 2.0, at
the root of this repository. Zeko-owned protocol-layer code under
[`src/app/zeko`](./src/app/zeko) is licensed under BUSL-1.1 with the Zeko
Additional Use Grant.

The current Change Date for Zeko-owned protocol-layer code is 2030-07-17, and
the Change License is Apache License, Version 2.0. Non-production/testnet use
is free under the Additional Use Grant. Independent Production Deployments of
the Zeko protocol layer require the self-serve commercial deployment license
unless an Additional Free Use applies.

This repository also publishes the shared self-serve framework used by protected
Zeko Agent Protocol Bundle repos and components that reference these terms.
Independent Agent Protocol Bundle production deployments are covered by the
self-serve commercial deployment license. The current published self-serve fee
is $0/year, subject to the pricing schedule in [PRICING.md](./PRICING.md).

Using or building on the Official Zeko Network or official Zeko-operated or
Zeko-authorized Agent Protocol Bundle services does not require a separate
commercial deployment license; users and integrators pay the ordinary network,
gas, transaction, prover, bridge, service, usage, marketplace, or similar fees
applicable to the official network and services.

Standard self-serve pricing is published in [PRICING.md](./PRICING.md):

- Protocol Layer Production Deployments: $0/year per production rollup under
  the current published self-serve pricing.
- Independent Agent Protocol Bundle Production Deployments: $0/year per
  deploying legal entity per Deployment Network under the current published
  self-serve pricing.

Current self-serve pricing is subject to change by a successor pricing schedule,
ecosystem exception, enterprise agreement, foundation agreement, or other
written authorization published or approved by Zeko Labs.

The self-serve commercial deployment license covers license rights only.
Managed deployment, enterprise support, compliance review, SLAs, custom
integrations, and dedicated infrastructure are separate commercial services.

See [LICENSING.md](./LICENSING.md), [src/app/zeko/LICENSE.md](./src/app/zeko/LICENSE.md),
[LICENSES/ZEKO-ADDITIONAL-USE-GRANT.md](./LICENSES/ZEKO-ADDITIONAL-USE-GRANT.md),
[COMMERCIAL-TERMS.md](./COMMERCIAL-TERMS.md), and [PRICING.md](./PRICING.md).
