# zkSecurity Audit: Zeko Circuits, Sequencer, and DA Layer

**Authoritative report:** [Audit of Zeko's circuits, sequencer and DA layer](https://reports.zksecurity.xyz/reports/zeko-2/)

**Repository:** [zeko-labs/zeko](https://github.com/zeko-labs/zeko)  
**Audited branch:** `compatible`  
**Audited revision:** `007b02e0cc`  
**Report date:** June 3, 2026

## Scope

This second audit is broader than the 2025 circuit audit. It reviewed the `compatible` branch and covered the following system areas.

### 1. Circuit changes since Audit 1

The circuit delta was reviewed under [src/app/zeko](https://github.com/zeko-labs/zeko/tree/compatible/src/app/zeko), including:

- The emergency DA rule and emergency DA folder.
- Changes to account-set insertion and indexed Merkle tree logic.
- Data Availability threshold-signature verification.
- Changes to normal commit and emergency-commit rules.

This is a delta review from Audit 1, not a claim that every unchanged circuit or bridge rule was re-audited from scratch.

### 2. Sequencer and proving pipeline

The audit also covered the sequencer and its operational paths, including:

- zkApp command logic.
- Provers and action-witness proof generation.
- Indexed Merkle tree and account-set handling.
- Transaction-snark proof merging.
- Commit processing.
- Recovery behavior.
- Emergency features and emergency-commit handling.

The primary area is [src/app/zeko/sequencer](https://github.com/zeko-labs/zeko/tree/compatible/src/app/zeko/sequencer), with supporting components under [src/app/zeko](https://github.com/zeko-labs/zeko/tree/compatible/src/app/zeko), including the prover, parallel-merger, indexed-Merkle-tree, and circuit areas.

### 3. Data Availability node

The DA-node review covered [src/app/zeko/da_layer](https://github.com/zeko-labs/zeko/tree/compatible/src/app/zeko/da_layer), including diff validation, sparse-ledger handling, storage, signatures, synchronization, and related archive-relay behavior.

### 4. Rollup deployment code

The deployment review included rollup deployment and permission configuration, including [src/app/zeko/sequencer/deploy.ml](https://github.com/zeko-labs/zeko/blob/compatible/src/app/zeko/sequencer/deploy.ml).

## Bridge split between the audits

Audit 1 directly covered the circuit-level bridge logic in `src/app/zeko/circuits/`, including the outer and inner bridge rules and their deposit, withdrawal, cancellation, and messaging paths.

Audit 2 did not present itself as a standalone full re-audit of every bridge rule. It covered bridge-related behavior where it intersects the newly reviewed sequencer, prover, commit, recovery, emergency, and DA paths. The report specifically discusses bridge withdrawals, action-state witnesses, and the effect of DA availability and sequencer behavior on those flows.

## Coverage limitation

This remains a component-level assessment. It does not constitute an audit of the entire `/zeko` repository or the `zeko-labs` GitHub organization. The authoritative report defines the final scope, threat model, assumptions, findings, and client responses.

See the [full report](https://reports.zksecurity.xyz/reports/zeko-2/) for details.
