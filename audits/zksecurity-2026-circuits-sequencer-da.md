# zkSecurity Audit: Zeko Circuits, Sequencer, and DA Layer

**Authoritative report:** [Audit of Zeko's circuits, sequencer and DA layer](https://reports.zksecurity.xyz/reports/zeko-2/)

**Repository:** [zeko-labs/zeko](https://github.com/zeko-labs/zeko)  
**Audited branch:** `compatible`  
**Audited revision:** `007b02e0cc`  
**Report date:** June 3, 2026

## Scope

This second audit is broader than the 2025 circuit audit. It covered:

- Circuit changes since the previous audit, including the emergency DA rule and folder, account-set insertion logic, DA threshold-signature verification, and changes to the commit and emergency-commit rules.
- The sequencer.
- The Data Availability (DA) layer.
- Rollup deployment code.

The corresponding repository areas are primarily:

- `src/app/zeko/circuits/`
- `src/app/zeko/sequencer/`
- `src/app/zeko/da_layer/`
- Deployment code, including `src/app/zeko/sequencer/deploy.ml`.

## Coverage limitation

This is still a component-level assessment. It does not constitute an audit of the entire `/zeko` repository or the `zeko-labs` GitHub organization. The authoritative report defines the final scope, threat model, assumptions, findings, and client responses.

See the [full report](https://reports.zksecurity.xyz/reports/zeko-2/) for details.
