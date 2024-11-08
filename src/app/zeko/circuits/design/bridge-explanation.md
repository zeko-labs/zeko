# Token transfer/bridge contract

The transferring/bridging of tokens is done in a separate contract
entirely.
The gist is that an outer account works as a bank,
which when deposited to, mints corresponding promissory
notes from a corresponding inner account.

These accounts are separate from the outer and inner account of the rollup.
We will thus refer to this new pair of accounts as the token outer and inner accounts.
For disambiguation purposes, the ones for the rollup are prefixed with rollup.

## Deposits

Deposits are made by posting an action on the rollup outer account,
along with sending the funds to the token outer account (verified by proof).
The user specifies an upper bound (slot) after which point if not
processed, the deposit will timeout and the funds will be recoverible.
The action will be a Witness action that witnesses the deposit to the
token outer account.

As explained above, on commit, the sequencer will also post an action on the
rollup outer account that details what kind of commit we made, along with the
slot bounds for the commit itself.

On the inside, the user can finalize a deposit that has been accepted.
A user deposit is accepted if there is a "commit" action after it
with suitable slot bounds such that the upper bound on the commit is less
than the timeout,
AND if there is no "time" action between them, which like a
"commit" action, is posted alongside its slot bounds, such that
the timeout is less than the lower bound of the "time" action.

We must however prevent double spends, thus the user must also provide
their helper account in the context (as a child),
and prove that they haven't processed it already.
This is done by storing in the account the index of the deposit last processed.
The index stored must be less than the index of the new deposit to be finalized.
After this, the index is updated to be the index of the new deposit.
Notably, it is possible to "skip" a deposit erroneously, but it is on the user
not to do this accidentally.

In this process we must match on the rollup outer action state as stored
in the inner account. This value can change, and cause the preconditions
to fail, but this is of no worry since failed transactions are feeless on Zeko.

The funds are then minted or sent by the token inner account,
depending on what kind of token it is.

## Withdrawals

To do withdrawals, we similarly post an action on the rollup inner action state.
We use the Witness action to show that we've either burned or sent the tokens
back to the token inner account.

To withdraw, a constant number of slots must have roughly passed since the
withdrawal was added. We figure out when a withdrawal was processed
by using the Commit actions added by the sequencer.
It does not matter which Commit action included it first.

Given this, the user can prove an upper bound for when their withdrawal was added
by taking some commit that includes their action and using its upper slot bound.
There is however an issue here:
As with the deposit case, we must match on the rollup inner action state stored on the
rollup outer account's app state.
This might change, invalidating the transaction.
Currently we don't work around this.
FIXME: fix #177.

As in the deposit case, we need to prevent double spends, thus similarly,
we have helper accounts on the outside too.
As with deposits, the helper account keeps track of the index of the last withdrawal
processed.
We also have to consider emergency changes.
The rollup inner action state might "roll back" and procede in another direction
due to this, and this is why we store the inner action state in the outer
account explicitly instead of just as an action.
This can happen via e.g. governance.
In addition to proving that it is contained in the rollup inner action state stored
in the rollup outer action state's "commit" action, we must prove that it's
also contained in the inner action state recorded in the outer app state.

## Cancelled deposits

There is however one more kind of transfer:
Cancelled deposits.
A cancelled deposit can be finalized analogously to deposits,
but on the outside, by proving that the action corresponding to the
deposit has been followed by an "time" action, which lower slot bound
exceeds the timeout slot, while no "commit" action which upper bound is less than
the timeout slot precedes it but comes after the deposit's action.

Analogously, to prevent double spends, we must keep track of this.
We use the same helper account as for withdrawals,
thus the helper account on the outside keeps track of two indices,
one for the index of the last withdrawal processed, and
one for the index of the last cancelled deposit processed.

## Governance (separate)

For a non-canonical bridge zkapp, governance is separate.
There is however the issue that the token inner account and
token outer account should be changed at the same time.
We can't ensure this, but we can instead have both circuits
verify that the vk of the other side is what it should be.
This is possible since every account update necessarily includes
the vk hash used as a precondition.
On the token outer account side, when a withdrawal action is only
valid if the token inner account update included in the Witness action
uses the correct vk hash,
and the same the other way around, that is,
a deposit is only valid if done with the expected vk hash.

This however creates a dangerous limbo state:
What if the token inner account is updated first,
but we do a deposit to the token outer account?
Those funds would be irreceivably lost.
The new circuit could accept both the new vk hash
and the one before that, if it's not a security vulnerability.

The same can be done the other way around when doing withdrawals.

## Governance (shared)

If the same governance controls both the rollup and the bridge zkapp,
then we want both the rollup and bridge to be updated at the same time.
Note that governance can not access the full transaction commitment,
since this is not made available to zkapps.
Instead, you can capture a subset by creating a helper token owner that creates
its own helper token account.
The helper token owner can have as children each of the outer accounts,
and those account updates can have as children the helper token account,
with permissions set to Parents_own_token at the first level and inherit
at the second.

## Withdrawal failsafe (unimplemented)

Consider the possibility that there is a bug in our token outer circuit,
allowing the user to withdraw all the deposited funds without doing a corresponding
withdrawal on the inside.
We do this by creating _two_ (or more, possibly) token outer accounts.
When you deposit your funds, you choose which one.
Half the time, only one of them is usable.
Specifically, half the time, the send permission will be Impossible,
and the vk will allow changing the vk (and permission) back during the other
portion of the time.
The other vk will implement similar logic, failing to work in the period when it
should be Impossible, transferring control back and resetting the permission.

Notably, you can still _deposit_ even when the send permission is Impossible.
You just can't withdraw from it.

Effectively, this means during a hack, if someone finds a vulnerability
in the withdrawal-allowing vk, it will only affect one of the accounts.
The other account is a separate circuit, and will need a separate vulnerability
to compromise.
It is much simpler, however, heavily reducing the surface for such bugs.
