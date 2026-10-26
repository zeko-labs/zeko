# Security Audits

This directory records external security assessments of specific Zeko components. The reports remain hosted by [zkSecurity](https://zksecurity.xyz/); these files provide stable links and scope notes for this repository.

These assessments are **not** audits of the entire `zeko-labs` GitHub organization. They are also not blanket audits of every directory in this repository. Each report applies to the code, branch, commit, and component scope stated below.

## Scope at a glance

| Report | Reviewed scope |
| --- | --- |
| [Audit 1](./zksecurity-2025-circuits/) | Direct review of Zeko circuit and bridge-contract logic in `src/app/zeko/circuits/`, plus circuit-relevant Mina changes. |
| [Audit 2](./zksecurity-2026-circuits-sequencer-da/) | Circuit changes since Audit 1, the sequencer and its prover/commit/recovery/emergency paths, the DA node, and rollup deployment code. It discusses bridge-related flows in those paths but is not a standalone re-audit of every bridge rule. |

## Rollup Architecture Coverage

Taken together, these two zkSecurity assessments cover the full Zeko-specific rollup architecture required to operate the audited mainnet design: circuits and bridge rules, transaction-snark and recursive proof logic, the sequencer and prover pipeline, indexed-Merkle/account-set handling, commit, recovery and emergency paths, DA nodes, archive-relay interactions, and rollup deployment/configuration.

Within this audited architecture, no additional Zeko-specific rollup subsystem is required to run the system beyond the components listed above. This is an architecture-coverage statement, not a claim that every file, external dependency, operational service, or later code revision was audited. Each report remains limited to its stated branch, commit, and scope.

## Open Source Protocol Development

Zeko is open-source protocol software. Teams are welcome to inspect, fork, extend, operate, and contribute to the protocol, including its circuits, bridge rules, sequencer, provers, DA layer, recovery paths, and deployment tooling.

Independent teams may adapt these protocol-development features for their own deployments and experiments, subject to the applicable repository licenses, third-party notices, and trademark requirements. The audit reports describe specific reviewed revisions and should not be read as a guarantee about later modifications or independent deployments.

## Reports

Each report directory includes an unofficial paraphrased README for agents, a complete finding index with source links, and a client-response summary focused on High findings. Detailed scope notes remain linked from each README. Original report text and diagrams remain at zkSecurity; this repository does not relicense those materials. Every High finding has an individual client-response summary and is labeled resolved according to its published response: seven in Audit 1 and four in Audit 2. Neither report lists Critical findings.

- [Audit of Zeko's circuits (May 2025)](./zksecurity-2025-circuits/)
- [Audit of Zeko's circuits, sequencer and DA layer (June 2026)](./zksecurity-2026-circuits-sequencer-da/)
