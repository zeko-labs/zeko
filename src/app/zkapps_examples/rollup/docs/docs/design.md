---
sidebar_position: 2
---

# Design of zkApp for rollup MVP

## Basic functionality

Field 0 of the app state is the hash of the current state of the ledger.
The ledger can be updated by providing a transaction statement
and its proof.

We need to support transfers to and from the rollup,
with and without cooperation from the sequencers.

One fundamental constraint is that zkApp commands have a complexity limit,
i.e. you can not have an infinite amount of account updates in a single command
(for obvious reasons).

We also disregard non-Mina tokens for now.

The L1 and L2 are connected by a special account on the L2,
denoted by having the account index 0.

The special account has minimal special treatment.
The transaction snark is modified such that this account
must have a specific vk set,
and the permissions must be all Proof.

The complete state of the account is exposed through requests and responses.

The account must start with the maximum amount of Mina possible when the rollup is made.
When a transfer happens from L1 to L2, an action is added to the L1 account, along with MINA.
The sequencer will also sequence these actions and keep track of a merkle list of
orders and merkle list of processed orders.
These fields will be stored on the L2 account.
On the other end, the L2 account will allow withdrawing MINA if there is such
an unprocessed order, and the processed list field must be updated accordingly.
The L1 account contract will need to check that these match.

In the other direction, MINA can be added to the L2 account along with an action
to go the other way.
On the L1 side this will allow you to withdraw from the account, and update the field
of processed withdrawals at the same time.

Field 0 of the app state of the L2 account is reserved to be the protocol state hash of
the entire L1.

This is implemented by in the zkapp command logic checking whether the account in question
is of the public key in question. The public key in question is fetched through a request.
If it is the specified account then check that the call data of the account update
is equivalent to the call data also fetched via a request.

In essence, we do a cross-layer contract call.

What if we have multiple such account updates in a single L1 rollup update?
The above (almost) inherently limits it to only one, since there is only
one possible call data.
If they both use the same call data, it could happen, but the contract would
be such that is impossible, e.g. by using a counter.

What about if we have none? We run into the problem that "all account updates
for the account" having that call data is _vacuously_ true.
We avoid this by checking that the nonce has been incremented by exactly once,
also avoiding the above problem.

This perhaps isn't optimal, since we might run into situations where we want multiple
L2 account updates per "commit" to the L1.

---

Do note that since the L2 has fewer/no limits, it is fast to go from L1 to L2,
but slow to go from L2 to L1, inherently.
The sequencer is also the one expected to do many of the operations on the L2 account,
and will thus also pay those fees, but we will ignore that for now since the fees should
be low enough that it should not deter profit, but we have to think about the incentives
of this.

---

What about all the other conditions on the L2 account? E.g. permissions set to proof and such.
That is part of the _initialisation_. Currently, initialisation isn't
guarded by anything, but before using a rollup, you can check the state of the rollup,
or otherwise check its history, to verify for yourself that it is legitimate.
If it's been made illegitimately, you can choose not to use it.
Although we could still enforce proper initialisation in the future if we wanted to.

## Implementation

There are three circuits, one for the nested account, one for the L1 account, and one for merging transaction snarks.
Why another circuit for merging transaction snarks? We ignore first/second-pass logic, since it is not relevant for our
purposes.

The logic for transfers is almost equivalent for the two accounts.
The action state for one account is synchronised to state field 0 of the other account.
State field 1 is the hash of the merkle list of actions processed.
A transfer from L1 to L2 is done by appending an action, adding the necessary amount to the outer account,
then when "sequencing", we synchronise the fields.
For each such sequencing, there can be exactly one "synchronisation" account update for the nested account,
since the account nonce must be incremented (enforced by the circuit), and that is checked in the outer account's
circuit.

At the same time as doing the synchronisation, some amount of payments is done.
For the nested account update, this is likely to be as many as there are to process,
but for the outer one, this is likely to be less, meaning a queue forms.
