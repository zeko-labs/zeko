# Audit 2: Circuits and Rollup Operations

Unofficial, paraphrased reading guide prepared by Zeko for humans and agents. The [authoritative zkSecurity report](https://reports.zksecurity.xyz/reports/zeko-2/) controls; this guide does not reproduce or relicense it. Consult the source for technical details and recommendations.

Published June 3, 2026. Reviewed `compatible` revision: [`007b02e0cc`](https://github.com/zeko-labs/zeko/commit/007b02e0cc).

## Scope and Paths

Circuit changes, sequencer/proving, DA nodes and deployment are covered. Bridge-related findings concern withdrawal availability and proof requests. [Detailed scope](../zksecurity-2026-circuits-sequencer-da.md) maps zkApp processing, provers, indexed trees, merging, commits, recovery and emergency paths. [Audit 1](../zksecurity-2025-circuits/) covers the earlier bridge circuits.

## Findings

IDs and severities follow the report; issue labels are paraphrased. Client responses for Critical and High findings are summarized below.

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

## Client Responses: Critical and High

Neither report classifies a finding as Critical. For this report's four High findings, the published client responses describe these fixes:

- **00:** Included actions in DA diffs and checked action-state updates ([PR #397](https://github.com/zeko-labs/zeko/pull/397)).
- **01:** Added account-set transitions to emergency folding and required both ledger and account-set paths in emergency DA ([PR #398](https://github.com/zeko-labs/zeko/pull/398)).
- **02:** Required equal source and target ledger hashes at initialization ([PR #395](https://github.com/zeko-labs/zeko/pull/395)).
- **03:** Recomputed sparse-ledger hashes without trusting cached intermediate hashes ([PR #396](https://github.com/zeko-labs/zeko/pull/396)).

These summarize published client responses; current deployments were not independently checked. Each finding ID above links to the original response.

## Original Diagrams

- [Emergency transaction reordering](https://reports.zksecurity.xyz/img/reports/zeko-2/reordering.png): alternative histories produce different account-set orderings.
- [Discarded diff path](https://reports.zksecurity.xyz/img/reports/zeko-2/ignored_diff.png): a shared ledger target hides an alternative history.

Diagrams remain hosted by zkSecurity. Their associated findings above provide the original explanation. This guide omits exploit walkthroughs and source code; use the linked findings when investigating an issue.
