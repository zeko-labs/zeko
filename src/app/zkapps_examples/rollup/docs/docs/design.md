---
sidebar_position: 2
---

# Zeko internal design

Zeko is meant to be a zkApp that contains a nested Mina ledger in its account state.
Transfers of Mina (only, for now) between the _outer_ and _inner_ ledgers should be possible.
These are the only goals for the MVP.

The design is meant only to do this as well as possible.
The MVP zkApp neither has any data availability checks to ensure that the witnesses
necessary to operate on the zkApp are available in the network.

## Circuits

- Outer circuit (for L1 zkApp)
- Inner circuit (for handling transfers on the L2 side cleanly)
- Transaction wrapper circuit (for wrapping transaction snarks)
- Action state extension circuit (for proving one action state is an extension of another)
- Helper token owner circuit for recording transfers processed

## zkApp design

The L1 and L2 are connected by a special account on the L2,
denoted by having a special public key.
The L1 account is called the outer account, and the L2 one the inner account.

The outer account keeps track of the ledger hash of the inner ledger,
the hash of the merkle list for all withdrawals (L2 -> L1).
Actions on the outer account are treated as deposits (L1 -> L2).

The inner account keeps track, similarly, of all deposits.

Updating the ledger hash can be done via supplying a valid ledger transition,
i.e. transaction snark.

The inner account must start with the maximum amount of Mina possible when the rollup is made.
When a deposit happens, an action is added to the outer account, along with MINA.
On the inner side the recipient can finalize the deposit, taking the MINA from the L1 account.

For withdrawals (L2 -> L1), the same happens in reverse.

### Processing transfers

The central problem we face is that we can not send MINA trivially.
MINA accounts may have `receive != None`, and we can not prevent them
from emptying their accounts, meaning the sequencer's submitted transaction
would fail, DoSing the rollup.

There _are_ ways to solve this with `receipt_chain_hash`, but
1) They are complicated.
2) They can not work when the `set_verification_key` permission is `Signature` or `Either`.

We opt to instead push this responsibility onto the user.
The user can finalize the transfer if they can prove it hasn't happened before.
In a sense, we're delegating the responsibility of storing this state to the user,
and must enforce they store it correctly.
We can do it through token accounts, with a token owner that only allows storing valid data.
We can do it by checking `receipt_chain_hash` at transfer-processing time.
We can also use simpler indicators, such as `is_new`, after all, a new account can
not have processed any transfers before!

#### Token approach

A transfer has two stages:
- Submission (add action)
- Processing (move funds from Zeko account to user account)

During the processing, we must prove that we haven't processed the transfer already,
by including a helper account specific to the recipient.
The helper account tracks the action state at the time of the last transfer processing.
The account update for the inner/outer account includes this as a child,
and updates the action state to the current one.
A proof that the actions between these two action states,
filtered by recipient, total to the amount, must be included.
If the account is new, the action state is regarded as being the empty action state.

In essence, we are tracking per-recipient what transfers have been processed.
We delegate the responsibility of this to the recipient themself.
How do we prevent them from tampering with the data?
We make the account have a special token id, which circuit _only_ allows transactions
in which they are subordinate to the inner/outer account.

Thus, they can never make an update which isn't also underneath the inner/outer account.

To simplify this, we make the inner/outer account the token owner, removing
the need for tracking the public key of the token owner.
