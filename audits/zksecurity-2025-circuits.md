# zkSecurity Audit: Zeko Circuits

**Authoritative report:** [Audit of Zeko's circuits](https://reports.zksecurity.xyz/reports/zeko/)

**Repository:** [zeko-labs/zeko](https://github.com/zeko-labs/zeko)  
**Audited revision:** `fab6a03555`  
**Report date:** May 21, 2025

## Scope

The audit primarily covered:

- `src/app/zeko/circuits/`, including Zeko's outer contracts and inner contracts.
- The diff from the original Mina repository where it affected Zeko's circuits, especially the transaction snark.

## Coverage limitation

This was a component-level circuit assessment, not an audit of the entire `/zeko` repository. The report states that the wider integration context and, in many places, witness-generation code were outside the practical scope at the time of review.

See the [full report](https://reports.zksecurity.xyz/reports/zeko/) for findings, recommendations, client responses, and the complete scope statement.
