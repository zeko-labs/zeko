# Token transfer/bridge contract

The transferring/bridging of tokens is done in a separate contract
entirely.
The gist is that an outer account works as a bank,
which when deposited to, mints corresponding promissory
notes from a corresponding inner account.

These accounts are separate from the outer and inner account of the rollup.
We will thus refer to this new pair of accounts as the token outer and inner accounts.
For disambiguation purposes, the ones for the rollup are prefixed with rollup.

## Bridge proof fee

The sequencer is the party that ultimately spends compute proving the
finalize/cancel transactions. To pay it for that work, every bridging
request bakes a fixed `bridge_proof_fee` into the transaction so that
the sequencer's `bridge_fee_recipient` accounts collect a fee whether the
request is a deposit, a withdrawal, or a cancelled deposit.

The fee is enforced inside the circuits, not by the sequencer's choice,
so a malicious sequencer cannot redirect the funds:

- On submitDeposit/submitWithdrawal, the deposit/withdrawal action that the
  user signs already carries a sibling account update
  paying `bridge_proof_fee` to `bridge_fee_recipient_l1` (deposit) or
  `bridge_fee_recipient_l2` (withdrawal). The user's transferrer must
  cover `amount + bridge_proof_fee`.
- On the finalize\* actions, the action update on the holder/zeko account
  has two additional child account updates appended to its calls: one paying
  `(amount - bridge_proof_fee)` (or, if the helper account is new, also minus
  the helper's account creation fee) to the recipient, and one paying
  `bridge_proof_fee` to the bridge fee recipient.

Because the user pays `amount + bridge_proof_fee` on submit but only
receives `(amount - bridge_proof_fee)` on finalize, the sequencer's
`bridge_fee_recipient` ends up with `2 × bridge_proof_fee` per completed
bridging request — one half collected at submit time, one half at
finalize time.

### Pre-signing the helper account update

Finalize transactions go through the sequencer, but the user authorizes
the helper account update with their own signature (replay-protected by
incrementing the helper's nonce). To make pre-signing safe — i.e. signing
before knowing who pays the fee_payer fee — the helper update sets
`use_full_commitment = false`. That way the user's signature commits only
to the account-updates hash, not to the memo/fee_payer hash, so the
sequencer can wrap the proven forest in any fee-payer of its own without
invalidating the signature. Replay is prevented by the constant-nonce
precondition + `increment_nonce = true` on the helper update.

## Deposits

Deposits are made by posting an action on the rollup outer account,
along with sending the funds to the token outer account (verified by proof).
The user specifies an upper bound (slot) after which point if not
processed, the deposit will timeout and the funds will be recoverible.
The action will be a Witness action that witnesses the deposit to the
token outer account.

The `Witness` action also includes a sibling fee-payout account update
that sends `bridge_proof_fee` to `bridge_fee_recipient_l1` from the
parent's token. This is what compensates the sequencer for proving and
posting the corresponding finalize-deposit transaction later.

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

The helper account update is signed by the user with the partial
transaction commitment (`use_full_commitment = false`), increments its own
nonce, and pins down its old nonce via a constant-nonce precondition. This
way the user can pre-sign the update without knowing the fee_payer, and the
nonce increment plus precondition keeps the signature single-use.

In this process we must match on the rollup outer action state as stored
in the inner account. This value can change, and cause the preconditions
to fail, but this is of no worry since failed transactions are feeless on Zeko.

The funds are then minted or sent by the token inner account,
depending on what kind of token it is. They're not sent to a recipient of
the sequencer's choosing — the finalize circuit hardcodes two sibling
account updates inside the action update: one for the recipient (for
`amount - bridge_proof_fee`, minus another `account_creation_fee` if the
helper account is new) and one for the bridge fee recipient (for
`bridge_proof_fee`). Because both are children of the action update with
no authorization of their own, they ride along with the proof and the
sequencer can't redirect or skip them.

## Withdrawals

To do withdrawals, we similarly post an action on the rollup inner action state.
We use the Witness action to show that we've either burned or sent the tokens
back to the token inner account. As with deposits, the `Witness` action also
includes a sibling fee-payout account update paying `bridge_proof_fee` to
`bridge_fee_recipient_l2` (from the parent's token) so the sequencer is
compensated for the corresponding finalize-withdrawal transaction it'll
post on L1 later.

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
processed. The helper update is signed with `use_full_commitment = false`
and `increment_nonce = true`, with a constant-nonce precondition, so the
user can pre-sign without knowing the fee_payer and replays are blocked.

The finalize-withdrawal circuit also appends two sibling payout account
updates to the action update: one to the recipient for
`amount - bridge_proof_fee` (or further minus `outer_account_creation_fee`
if the helper account is new), and one to `bridge_fee_recipient_l1` for
`bridge_proof_fee`. Same as in the deposit case, these are determined by
the proof and not by the sequencer.
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

The same fee-payout treatment applies as in the withdrawal case: the
helper signature uses the partial commitment (with constant-nonce
precondition + `increment_nonce`), and the finalize-cancelled-deposit
circuit appends a recipient payout (`amount - bridge_proof_fee`, minus
`outer_account_creation_fee` if the helper is new) and a fixed
`bridge_proof_fee` payout to `bridge_fee_recipient_l1` as siblings of the
action update.

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

## Withdrawal failsafe

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
