# Mina Mesa upgrade plan

## Decision

Zeko will port the Mesa protocol changes onto its `compatible` fork instead of
merging the current Mina `compatible` or `release/mesa` branches. The common
history predates several thousand upstream commits, so a bulk merge has an
unacceptably large semantic review surface even where Git reports few textual
conflicts.

The upgrade targets:

- 32-field zkApp state and the corresponding stable wire versions;
- 90-second Mina L1 slots;
- preservation of the existing Zeko L2 ledger;
- a coordinated maintenance-window cutover;
- full migration rehearsal against Mesa testnet.

Zeko already removes the old aggregate zkApp size check. Mesa event/action and
account-update wire formats must still decode correctly, but no additional
Zeko policy limit is introduced.

## Implementation

### Protocol types and circuits

Port the upstream Mesa state-size and stable-version changes while retaining
all `ZEKO NOTE` behavior. Replace hardcoded `Vector_8` uses in Zeko helpers with
`Zkapp_state.V`. Existing rollup and bridge state occupies the same leading
fields; fields 8 through 31 are `Keep`, `Ignore`, or zero as appropriate.

The Mina account, account-update, precondition, zkApp-command, user-command,
and transaction-witness stable versions must match Mesa. This changes every
Zeko circuit that embeds those types. Regenerate all real verification keys,
proof caches, constraint-system hashes, and deployment artifacts.

### Slot handling

The sequencer derives the current Mina slot from the hard-fork timestamp,
hard-fork slot, and a configured 90-second slot duration. Time-based command
acceptance is converted using that duration. Slot-count configuration remains
explicit; production values must be doubled where the desired wall-clock
window is unchanged from Berkeley.

Timed Mina accounts require the upstream slot-reduction migration so cliff and
vesting schedules preserve their wall-clock meaning.

### Ledger and persistence migration

The offline migration runs only while sequencer, prover, DA, and commit intake
are stopped.

1. Verify the source ledger root against the expected Berkeley root.
2. Pad every zkApp account from 8 to 32 state fields.
3. Apply the timed-account slot-reduction migration.
4. Preserve account indices and rebuild the account-set IMT.
5. Write a new checkpoint plus a manifest containing source/target roots,
   account count, hard-fork slot, and zkApp-state size.
6. Start DA from a new database/epoch using Mesa account and user-command
   encodings. The targeted port does not preserve in-process decoding of
   Berkeley DA records because Mina removed the old account stable type;
   retain the old binary and database for historical access.
7. Clear pending Berkeley proofs, merger jobs, and witnesses.

Padding account state changes account hashes and the ledger root. The cutover
must therefore update the L1 outer rollup state from the exact old root to the
manifest's new root through the authorized state-update path. Action pointers
and the account-set root remain unchanged. DA starts a new epoch rooted at the
migrated ledger hash; pre-Mesa data remains read-only.

## Cutover

1. Pause command intake and drain or reject pending work.
2. Produce a final Berkeley commit and checkpoint.
3. Install Mesa verification keys on all outer, bridge, holder, and token-owner
   accounts with the existing multisig upgrade tooling.
4. Run and independently verify the L2 ledger migration.
5. Install Mesa L2 account verification keys.
6. Apply the exact-root outer-state transition.
7. Initialize the Mesa DA epoch and obtain quorum signatures.
8. Start Mesa sequencer/provers and resume intake.

Rollback is allowed only before the outer-state root transition. After that
transition, rollback requires a separately authorized reverse migration and
must not reuse Berkeley proofs or DA signatures.

## Acceptance tests

- Build the sequencer, DA layer, signer, Zeko circuits, and real prover.
- Verify stable/wire round trips for Berkeley historical values and Mesa
  current values.
- Verify deterministic 8-to-32 padding and timed-account migration.
- Verify 90-second current-slot and command-validity calculations.
- Rehearse from a Berkeley checkpoint on Mesa testnet, including verification
  key rotation, migration, restart, deposits, withdrawals, DA diffs, a zkApp
  touching a field above index 7, transaction proofs, merges, and an L1 commit.
- Require two independent migration runs to produce the same target root and
  manifest before production promotion.
