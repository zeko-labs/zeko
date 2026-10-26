# Audit 2: Circuits and Rollup Operations

Unofficial, paraphrased reading guide prepared by Zeko for humans and agents. The [authoritative zkSecurity report](https://reports.zksecurity.xyz/reports/zeko-2/) controls; this guide does not reproduce or relicense it. Consult the source for technical details and recommendations.

Published June 3, 2026. Reviewed `compatible` revision: [`007b02e0cc`](https://github.com/zeko-labs/zeko/commit/007b02e0cc).

## Scope and Paths

Circuit changes, sequencer/proving, DA nodes and deployment are covered. Bridge-related findings concern withdrawal availability and proof requests. [Detailed scope](../zksecurity-2026-circuits-sequencer-da.md) maps zkApp processing, provers, indexed trees, merging, commits, recovery and emergency paths. [Audit 1](../zksecurity-2025-circuits/) covers the earlier bridge circuits.

## Findings

IDs and severities follow the report; labels are paraphrased. **R** means the published response reports remediation; **U** means no client response is published. Neither label independently establishes current deployment status.

| ID | Severity | Paraphrased issue | Response |
| --- | --- | --- | --- |
| [00](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-l2-actions-not-stored-in-da) | High | Missing action openings impede withdrawals | R: [397](https://github.com/zeko-labs/zeko/pull/397) |
| [01](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-transaction-reordering) | High | Emergency ordering obscures account-set reconstruction | R: [398](https://github.com/zeko-labs/zeko/pull/398) |
| [02](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unconstrained-da-folder-init) | High | Arbitrary folding start state | R: [395](https://github.com/zeko-labs/zeko/pull/395) |
| [03](https://reports.zksecurity.xyz/reports/zeko-2/#finding-high-unsafe-deserialization-sparse-ledger) | High | Untrusted cached ledger hashes | R: [396](https://github.com/zeko-labs/zeko/pull/396) |
| [04](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-censor-archive-commands) | Medium | Command omission undermines archiving | U |
| [05](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-duplicated-diffs-da) | Medium | Alternate diff paths discarded | U |
| [06](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-integer-overflow-index-of-path) | Medium | Ledger index exceeds arithmetic width | R: [399](https://github.com/zeko-labs/zeko/pull/399) |
| [07](https://reports.zksecurity.xyz/reports/zeko-2/#finding-medium-sequencer-dos) | Medium | Unrestricted proof requests exhaust resources | R: [408](https://github.com/zeko-labs/zeko/pull/408), [412](https://github.com/zeko-labs/zeko/pull/412) |
| [08](https://reports.zksecurity.xyz/reports/zeko-2/#finding-low-da-observations) | Low | DA synchronization/authentication gaps | U |
| [09](https://reports.zksecurity.xyz/reports/zeko-2/#finding-low-permissions) | Low | Signing keys bypass proof controls | U |
| [0a](https://reports.zksecurity.xyz/reports/zeko-2/#finding-info-quorum-underflow) | Informational | Zero quorum blocks commits | U |

## Original Diagrams

- [Emergency transaction reordering](https://reports.zksecurity.xyz/img/reports/zeko-2/reordering.png): alternative histories produce different account-set orderings.
- [Discarded diff path](https://reports.zksecurity.xyz/img/reports/zeko-2/ignored_diff.png): a shared ledger target hides an alternative history.

Diagrams remain hosted by zkSecurity. Their associated findings above provide the original explanation. This guide omits exploit walkthroughs and source code; use the linked findings when investigating an issue.
