# Audit 1: Circuits and Bridge Rules

Unofficial, paraphrased reading guide prepared by Zeko for humans and agents. The [authoritative zkSecurity report](https://reports.zksecurity.xyz/reports/zeko/) controls; this guide does not reproduce or relicense it. Consult the source for technical details and recommendations.

Published May 21, 2025. Reviewed revision: [`fab6a03555`](https://github.com/zeko-labs/zeko/commit/fab6a03555).

## Scope and Paths

Reviewed `src/app/zeko/circuits/`, including inner/outer bridge rules, and circuit-relevant Mina changes. Wider integration and incomplete witness-generation code were excluded. See the [detailed scope and bridge mapping](../zksecurity-2025-circuits.md).

## Findings

IDs and severities follow the report. Labels paraphrase findings. **R** means the published client response reports a correction; **D** means initialization requirements were documented; **U** means no client response is published. These describe the report, not current deployment status.

| ID | Severity | Paraphrased issue | Response |
| --- | --- | --- | --- |
| [00](https://reports.zksecurity.xyz/reports/zeko/#finding-high-00-missing-merge-consistency) | High | Disconnected recursive transitions | R |
| [01](https://reports.zksecurity.xyz/reports/zeko/#finding-high-01-unconstrained-account-set-update) | High | Unbound account-set root | R |
| [02](https://reports.zksecurity.xyz/reports/zeko/#finding-high-02-missing-helper-token-owner) | High | Replay across bridge holders | R |
| [03](https://reports.zksecurity.xyz/reports/zeko/#finding-high-03-range-check-not-canonical) | High | Noncanonical comparison operand | R |
| [04](https://reports.zksecurity.xyz/reports/zeko/#finding-high-04-range-check-not-strict) | High | Equality passes strict comparison | R |
| [05](https://reports.zksecurity.xyz/reports/zeko/#finding-high-05-range-check-missing-lookups) | High | Unchecked limb slices | R |
| [06](https://reports.zksecurity.xyz/reports/zeko/#finding-high-06-no-withdrawal-delay) | High | Withdrawal/commit linkage missing | R |
| [07](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-01-missing-receive-rule-inner-bridge) | Medium | Bridge lacks receiving rule | R |
| [08](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-02-incorrect-slot-range-merge) | Medium | Incorrect time-window intersection | R |
| [09](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-03-transaction-snark-no-success) | Medium | Failed updates remain provable | R |
| [0a](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-04-initial-state-assumptions) | Medium | Initialization relies on assumptions | D |
| [0b](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-05-range-check-overconstrained-zero-carry) | Medium | Carry restriction rejects inputs | R |
| [0c](https://reports.zksecurity.xyz/reports/zeko/#finding-medium-06-mixed-bridge-rules-compile) | Medium | Bridge methods compiled together | R |
| [0d](https://reports.zksecurity.xyz/reports/zeko/#finding-low-hard-coded-decrement-constant) | Low | Decrement ignores parameter | U |
| [0e](https://reports.zksecurity.xyz/reports/zeko/#finding-info-custom-bridge-rules-error) | Informational | Custom-token configuration mismatch | U |

## Protocol and Figures

The source presents [messaging](https://reports.zksecurity.xyz/reports/zeko/#messaging) and [bridge behavior](https://reports.zksecurity.xyz/reports/zeko/#bridge-contracts) through prose and pseudocode; no report figure was identified. Follow those links for the original presentation.

[Audit 2](../zksecurity-2026-circuits-sequencer-da/) adds circuit changes and operational components; see its scope note for the bridge coverage split.
