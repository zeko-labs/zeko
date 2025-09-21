# Explanation

We wish to have a rollup.
A nested instantiation of the Mina ledger on top of itself,
as a zkApp account.
We wish to support communication between the "outside" (L1) and "inside" (L2).
It should be possible to transfer value.
It should be possible to upgrade the contract when the time comes.
It should be possible to have a stake in the rollup.
And above all, it should be secure.

To that end, here is a summary of what we want from the core protocol:

- The core rollup protocol does not handle transfer of value/MINA.
- There is an associated token called ZEKO, using the fungible
  token standard, minted on the L1.
- People pay fees in MINA to sequencer.
- Data availability is ensured by having a public key that must have
  signed the hashes used.
- Communication happens by posting actions to the zkApp account on the L1.
  This account is referred to as the _outer_ account.
  There is a corresponding _inner_ account on the L2,
  whereto you can post actions to communicate the other way.
  On commit, the action states are synchronized.
  Notably, the outer action state is synchronized _up to some point_,
  to ensure that sequencer does not waste work synchronizing something that
  might be rolled back immediately.
- Actions on the outside:
  - Witness (witness arbitrary account update)
  - Commit (sequencer committed)
- There is a backup special committee that can pause the rollup.
  Being paused is indicated by a field on the outer account.

How can we implement transfers of tokens on top of this?
Consider the coremost MINA case:
There is a special account on the L2 that is initialized with maximum MINA.
There is a corresponding account the L1, to which you can deposit MINA.
You witness the deposit, and can thus correspondingly take out the MINA on the L2.
Double spending is prevented by tracking a token account on the L2 the index of
the last deposit processed.
The index is the index of the witness action on the outer account in the merkle list
of actions.
Withdrawals happen correspondingly, the other way around.
We also wish to support timeouts on deposits.
We do this by regarding a deposit as having three states:

- Unknown
- Accepted
- Rejected

It starts by default as Unknown. Iterate through all actions that come after,
and if there is a Time action where the lower bound is higher than the timeout,
it's marked as Rejected.
If there is a Commit action that commits an action state that contains the deposit
before it times out (marked by a Time action), then it's marked as Accepted.

We handle cancelled deposits the same way as withdrawals.
The index of the last cancelled deposit withdrawn is stored in a token account
on the L1 as with for withdrawals.

In addition, withdrawal logic must assert that the rollup is not paused.

## Synchronization

We wish to support transfer of information between L1 and L2.
To that end we have the special inner account.
The inner account has a field which contains the outer action state.

The outer circuit checks whether the inner app state field matches
a predecessor of the outer action state.
However, inner app state may have been updated many times.
Thus, inner circuit also checks that stored
outer action state only moves forward.
Correctness of each individual step is implied by this.

We check that it matches a predecessor instead of the actual action state
such that sequencer can avoid making SNARKs that are invalidated in the event
of a rollback.

Consider, however, the case where the sequencer does not move
the recorded outer action state forward enough.
This would mean communication is delayed (and is one reason why timeouts are
important!).
We wish to incentivize sequencers to move the recorded action state forward
as much as possible.

This is already the case, in fact, since as a sequencer you want to
reap in as many fees as possible.
To that end it is optimal to move the action state forward at the very beginning,
such that people can process their withdrawals, use their funds, and pay
the sequencer fees.

### FIXME: Store multiple action states? (#177)

We only store _one_ of the action states,
meaning that if you don't prove fast enough you could
miss your chance to submit your transaction.

## Committing

On commit, the sequencer must present a transaction snark
that represents the transition from the old ledger hash
stored in the outer account to the new one.
It must also submit an action that contains the ledger hash
committed, along with the inner action state and processed outer
action state at the time of the commit.

Deposits (albeit external to the system) use this mechanism to
figure out if they've been rejected or not.

Thus, the commit must also include the slot range used at the time
of the commit.
If the slot range's upper bound is lower than the deposit's timeout,
then it must have been processed.

However, the sequencer doesn't always have any good incentive to choose
a tight bound, since choosing a tight bound means that _future_ sequencers
can profit from those deposits being processed.

To prevent this from happening, we also specify a maximum size for the slot range.

## Emergency commit

When the sequencer goes offline, we need a way to unblock the system
without decentralization of sequencing already in place.

An emergency commit may be issued by anyone once enough time has
passed since the upper bound of the last commit slot range.
That upper bound witnesses the latest possible time a commit could
have occurred; if the current slot is past it by a fixed margin,
then no commit has happened since.

Malicious sequencer can not pick a very large upper bound, since the slot range is capped by max_valid_size.

However, we can only certify “no commit happened” relative to one of
the last five outer action states. To make this check viable,
the sequencer must maintain rolling commits over at least five slots
within the max_sequencer_inactivity window. With a sufficiently
large window (e.g., on the order of a month), this obligation is
trivial for a healthy sequencer.

In effect, the emergency commit seals the gap with a bounded slot
range, restores liveness, and lets subsequent sequencers resume
committing under the usual rules.

## ZEKO token

The ZEKO token will use the
[fungible token standard](https://github.com/MinaFoundation/mina-fungible-token),
which, interestingly, is also an implementation.
This is to enable third parties to use the token without importing foreign code
to generate the proof; every token uses the same vk, so you only need to use known
code.

The rollup code doesn't interact with this directly yet.
Transfers of ZEKO will be handled very similarly to the case of MINA.

## Forced account update / governance (unimplemented)

(NB: permissions on account might be set to Either,
so this isn't necessarily the only way of doing a forced update).

## Our changes to transaction logic

We do not support processing failed transactions,
thus, you can not take fees from failed transactions.

Neither do we support any network preconditions currently.
We might support a subset in the future to allow checking the ledger hash.

We support time preconditions. (unimplemented, https://github.com/zeko-labs/zeko/issues/63)
There is however not a single global slot when proving transactions.
There is a range, and that range (or a tighter one) must be used as precondition when committing.

The sequencer should however not accept transactions that have tight time bounds,
since the commit would be likely to fail, wasting work potentially.

## Emergency pause

There is a public key that can pause the rollup.
The public key is stored in the app state and can be changed via governance.
