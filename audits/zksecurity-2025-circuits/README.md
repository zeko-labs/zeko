# Audit 1: Circuits and Bridge Rules

Unofficial, paraphrased reading guide prepared by Zeko for humans and agents. The [authoritative zkSecurity report](https://reports.zksecurity.xyz/reports/zeko/) controls; this guide does not reproduce or relicense it. Consult the source for technical details and recommendations.

Published May 21, 2025. Reviewed revision: [`fab6a03555`](https://github.com/zeko-labs/zeko/commit/fab6a03555).

## Scope and Paths

Reviewed `src/app/zeko/circuits/`, including inner/outer bridge rules, and circuit-relevant Mina changes. Wider integration and incomplete witness-generation code were excluded. See the [detailed scope and bridge mapping](../zksecurity-2025-circuits.md).

## Findings

IDs and severities follow the report; issue labels are paraphrased. Client responses for High findings are summarized below.

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

## Client Responses: High

The published report's client responses report fixes for all seven High findings.

- **[00](https://reports.zksecurity.xyz/reports/zeko/#finding-high-00-missing-merge-consistency): Resolved (per report).** Checked that the left proof's target equals the right proof's source.
- **[01](https://reports.zksecurity.xyz/reports/zeko/#finding-high-01-unconstrained-account-set-update): Resolved (per report).** Used the original account-set root when insertion is skipped.
- **[02](https://reports.zksecurity.xyz/reports/zeko/#finding-high-02-missing-helper-token-owner): Resolved (per report).** Added the shared helper-token owner.
- **[03](https://reports.zksecurity.xyz/reports/zeko/#finding-high-03-range-check-not-canonical): Resolved (per report).** Checked canonicality of the correct comparison operand.
- **[04](https://reports.zksecurity.xyz/reports/zeko/#finding-high-04-range-check-not-strict): Resolved (per report).** Range-checked the final decremented result.
- **[05](https://reports.zksecurity.xyz/reports/zeko/#finding-high-05-range-check-missing-lookups): Resolved (per report).** Returned the correct slices for range checks.
- **[06](https://reports.zksecurity.xyz/reports/zeko/#finding-high-06-no-withdrawal-delay): Resolved (per report).** Linked the withdrawal action-state extension to its containing commit.

Resolution status reflects the published client responses linked above.

## Protocol and Figures

The source presents [messaging](https://reports.zksecurity.xyz/reports/zeko/#messaging) and [bridge behavior](https://reports.zksecurity.xyz/reports/zeko/#bridge-contracts) through prose and pseudocode; no report figure was identified. Follow those links for the original presentation.

[Audit 2](../zksecurity-2026-circuits-sequencer-da/) adds circuit changes and operational components; see its scope note for the bridge coverage split.
