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

We make use of actions to maintain a queue of pending L1<->L2 interactions.
Interactions are currently limited to simple transfers between accounts.

To send money to an L2 account from an L1 account,
an action is created which notes the sender and the recipient.
The money must be deposited to the account of the rollup in the same
account update.

The sequencer will reduce the actions and modify the L2 state accordingly.
The transaction statement from the old ledger hash to the new ledger hash
must be parameterised by the actions, such that an L2 transaction can reference
an action, and for it to be included in the L1 account update, the actions referenced
by the merged L2 transaction (i.e. actions of all included L2 transactions)
must be reduced in that L1 account update.

The sender is included such that in the case of catastrophic failure
(no sequencer), the funds will be returned.

In the opposite direction, we can not use actions, as the accumulated withdrawals
from the L2 might exceed the zkapp command complexity limit.

We could choose to simply limit the number of withdrawals processed per account update,
but this has bad UX in times where many withdraw, since your L2 transaction would "float"
for a while before being accepted.

Instead, field 1 of the app state is a queue of pending withdrawals,
while field 2 is the hash of the portion of the queue processed.
When doing any interaction with the zkApp, the queue must have at least
one withdrawal popped off unless there is nothing to process.

In times of high activity, the queue will blow up, but will slowly be
reduced until all withdrawals have been processed.

## Catastrophic failure

If no account update has happened for N slots,
allow anyone to operate on the account, but rather than relying
on the DA layer, all data must be submitted as an event, i.e.
made public as part of the L1 transaction, such that you can rebuild
the state of the Merkle trees via archive nodes.

## Data availability

TBD
