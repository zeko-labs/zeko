During every macroslot we want to ensure that the sequencer
does not make conflicting commits.
We ensure this by locking up more than necessary,
and slashing it if the next sequencer (and only next sequencer!)
can prove that the previous sequencer committed to more or less than
1 ledger hash for the previous macroslot.

During each macroslot,
only the sequencer who won the macroslot can post commits.
At some point, they are to end their turn, which they do by
signing the final ledger hash for their turn.

After this point there will be no more commits until the next
macroslot.
The next sequencer can begin taking transactions and proving
using the signed ledger as the source ledger,
but he should take care not to commit until it's finalized.

If the last commit is no longer in the canonical chain,
then either:
- There has been no final commit the last macroslot,
- or there is another final commit for another ledger in the last macroslot.

In the former case, it's shown that no final commit has happened,
and the previous sequencer's right to withdraw their deposited ZEKO is
forfeit.

In the latter case, the sequencer shows that there is _another_
signature for the same macroslot, but for a different ledger,
and uses this to again forfeit the previous sequencer's right to
their deposited ZEKO.

The final commit should be done before the macroslot is over,
since the sequencer might be censored by the block producers
if the block producers choose to collude with competing sequencers.
In order to reduce such violent competition, it is required that
sequencers have a non-trivial stake in the protocol.
Of course this doesn't prevent violent competition between
different instantiations of ZEKO.
In the future it could be required that they have a good amount of MINA
too to encourage them to think of Mina as a whole.

After the final commit has been done, the next sequencer can begin committing early,
to avoid wasting work, and avoid downtime.
