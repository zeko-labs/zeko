# Audit 2: Circuits and Rollup Operations

**Authoritative report:** [zkSecurity Audit 2: Zeko's circuits, sequencer and DA layer](https://reports.zksecurity.xyz/reports/zeko-2/)

**Companion audit:** [Other authoritative report: Audit 1](https://reports.zksecurity.xyz/reports/zeko/) | [Audit 1 folder and README](../zksecurity-2025-circuits/README.md)

This README is an unofficial, paraphrased guide prepared by Zeko for humans and agents. The linked zkSecurity reports are authoritative for their respective scopes, findings, recommendations, and client responses. This guide does not reproduce or relicense them.

Published June 3, 2026. Reviewed `compatible` revision: [`007b02e0cc`](https://github.com/zeko-labs/zeko/commit/007b02e0cc).

## Scope and Paths

Circuit changes, sequencer/proving, DA nodes and deployment are covered. Bridge-related findings concern withdrawal availability and proof requests. [Detailed scope](../zksecurity-2026-circuits-sequencer-da.md) maps zkApp processing, provers, indexed trees, merging, commits, recovery and emergency paths. [Audit 1](../zksecurity-2025-circuits/) covers the earlier bridge circuits.

## Findings

IDs and severities follow the report; issue labels are paraphrased. Client responses for High findings are summarized below.

| ID | Severity | Paraphrased issue |
| --- | --- | --- |
| [00](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-l2-actions-not-stored-in-da) | High | Missing action openings impede withdrawals |
| [01](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-transaction-reordering) | High | Emergency ordering obscures account-set reconstruction |
| [02](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unconstrained-da-folder-init) | High | Arbitrary folding start state |
| [03](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unsafe-deserialization-sparse-ledger) | High | Untrusted cached ledger hashes |
| [04](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-censor-archive-commands) | Medium | Command omission undermines archiving |
| [05](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-duplicated-diffs-da) | Medium | Alternate diff paths discarded |
| [06](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-integer-overflow-index-of-path) | Medium | Ledger index exceeds arithmetic width |
| [07](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-sequencer-dos) | Medium | Unrestricted proof requests exhaust resources |
| [08](https://reports.zksecurity.xyz/reports/zeko-2/#finding-low-da-observations) | Low | DA synchronization/authentication gaps |
| [09](https://reports.zksecurity.xyz/reports/zeko-2/#finding-low-permissions) | Low | Signing keys bypass proof controls |
| [0a](https://reports.zksecurity.xyz/reports/zeko-2/#finding-info-quorum-underflow) | Informational | Zero quorum blocks commits |

## Client Responses: High

The published report's client responses report fixes for all four High findings.

- **[00](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-l2-actions-not-stored-in-da): Resolved (per report).** Included actions in DA diffs and verified action-state updates ([PR #397](https://github.com/zeko-labs/zeko/pull/397)).
- **[01](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-transaction-reordering): Resolved (per report).** Added account-set transitions to emergency folding ([PR #398](https://github.com/zeko-labs/zeko/pull/398)).
- **[02](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unconstrained-da-folder-init): Resolved (per report).** Required equal source and target ledger hashes during initialization ([PR #395](https://github.com/zeko-labs/zeko/pull/395)).
- **[03](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unsafe-deserialization-sparse-ledger): Resolved (per report).** Recomputed sparse-ledger hashes without trusting cached intermediate values ([PR #396](https://github.com/zeko-labs/zeko/pull/396)).

Resolution status reflects the published client responses linked above.

## Original Diagrams

- [Emergency transaction reordering](https://reports.zksecurity.xyz/img/reports/zeko-2/reordering.png): alternative histories produce different account-set orderings.
- [Discarded diff path](https://reports.zksecurity.xyz/img/reports/zeko-2/ignored_diff.png): a shared ledger target hides an alternative history.

Diagrams remain hosted by zkSecurity. Their associated findings above provide the original explanation. This guide omits exploit walkthroughs and source code; use the linked findings when investigating an issue.
