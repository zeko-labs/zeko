# Audit 1: Circuits and Bridge Rules

Unofficial, paraphrased reading guide prepared by Zeko for humans and agents. The [authoritative zkSecurity report](https://reports.zksecurity.xyz/reports/zeko/) controls; this guide does not reproduce or relicense it. Consult the source for technical details and recommendations.

Published May 21, 2025. Reviewed revision: [`fab6a03555`](https://github.com/zeko-labs/zeko/commit/fab6a03555).

## Scope and Paths

Reviewed `src/app/zeko/circuits/`, including inner/outer bridge rules, and circuit-relevant Mina changes. Wider integration and incomplete witness-generation code were excluded. See the [detailed scope and bridge mapping](../zksecurity-2025-circuits.md).

## Findings

IDs and severities follow the report; issue labels are paraphrased. Client responses for Critical and High findings are summarized below.

| ID | Severity | Paraphrased issue |
| --- | --- | --- |
| [00](https://reports.zksecurity.xyz/reports/zeko/#finding-high-00-missing-merge-consistency) | High | Disconnected recursive transitions |
| [01](https://reports.zksecurity.xyz/reports/zeko/#finding-high-01-unconstrained-account-set-update) | High | Unbound account-set root |
| [02](https://reports.zksecurity.xyz/reports/zeko/#finding-high-02-missing-helper-token-owner) | High | Replay across bridge holders |
| [03](https://reports.zksecurity.xyz/reports/zeko/#finding-high-03-range-check-not-canonical) | High | Noncanonical comparison operand |
| [04](https://reports.zksecurity.xyz/reports/zeko/#finding-high-04-range-check-not-strict) | High | Equality passes strict comparison |
| [05](https://reports.zksecurity.xyz/reports/zeko/#finding-high-05-range-check-missing-lookups) | High | Unchecked limb slices |
| [06](https://reports.zksecurity.xyz/reports/zeko/#finding-high-06-no-withdrawal-delay) | High | Withdrawal/commit linkage missing |
| [07](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-01-missing-receive-rule-inner-bridge) | Medium | Bridge lacks receiving rule |
| [08](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-02-incorrect-slot-range-merge) | Medium | Incorrect time-window intersection |
| [09](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-03-transaction-snark-no-success) | Medium | Failed updates remain provable |
| [0a](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-04-initial-state-assumptions) | Medium | Initialization relies on assumptions |
| [0b](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-05-range-check-overconstrained-zero-carry) | Medium | Carry restriction rejects inputs |
| [0c](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-06-mixed-bridge-rules-compile) | Medium | Bridge methods compiled together |
| [0d](https://reports.zksecurity.xyz/reports/zeko/#finding-low-hard-coded-decrement-constant) | Low | Decrement ignores parameter |
| [0e](https://reports.zksecurity.xyz/reports/zeko/#finding-info-custom-bridge-rules-error) | Informational | Custom-token configuration mismatch |

## Client Responses: Critical and High

Neither report classifies a finding as Critical. For this report's seven High findings, the published client responses describe these fixes:

- **00:** Linked recursive transitions by checking that the left target equals the right source.
- **01:** Corrected conditional account-set insertion checks.
- **02:** Added the shared helper-token owner ([commit](https://github.com/zeko-labs/zeko/commit/cf78d9b84efbfe93f02dddf9004c51d1b04b3bc8)).
- **03-05:** Corrected comparison canonicality, strictness, and range-checked slices.
- **06:** Bound withdrawal evidence to its commit through the action-state extension ([commit](https://github.com/zeko-labs/zeko/commit/c291d4d37e7fcf0008670f73a5de135c0e93e43d)).

These summarize published client responses; current deployments were not independently checked. Each finding ID above links to the original response.

## Protocol and Figures

The source presents [messaging](https://reports.zksecurity.xyz/reports/zeko/#messaging) and [bridge behavior](https://reports.zksecurity.xyz/reports/zeko/#bridge-contracts) through prose and pseudocode; no report figure was identified. Follow those links for the original presentation.

[Audit 2](../zksecurity-2026-circuits-sequencer-da/) adds circuit changes and operational components; see its scope note for the bridge coverage split.
