# Internal design of circuits

## Outline

We wish to have a rollup.
A nested instantiation of the Mina ledger on top of itself,
as a zkApp account.
We wish to support communication between the "outside" (L1) and "inside" (L2).
It should be possible to transfer value.
It should be possible to upgrade the contract when the time comes.
It should be possible to have a stake in the rollup.
And above all, it should be secure.

To that end, here is a summary of the core protocol:
- The core rollup protocol does not handle transfer of value/MINA.
- There is a sequencer auction on L1.
- There is an associated token called ZEKO, using the fungible
  token standard, minted on the L1.
- Auctions are bid in ZEKO.
- People pay fees in whatever currency they like to sequencer.
- Data availability is ensured by having a public key that must have
  sign the hashes used.
- Communication happens by posting actions to the zkApp account on the L1.
  This account is referred to as the _outer_ account.
  There is a corresponding _inner_ account on the L2,
  whereto you can post actions to communicate the other way.
  On commit, the action states are synchronised.
  Notably, the outer action state is synchronised _up to some point_,
  to ensure that sequencer does not waste work synchronising something that
  might be rolled back immediately.
- Actions on the outside:
  + Witness (witness arbitrary account update)
  + Bid (sequencer bid)
  + Commit (sequencer committed)
  + Time (match time, can be emulated by Witness
    technically, included for efficiency of token transfer)
- Governance is done tallying votes based on how much ZEKO
  the voter holds on the L1 and L2 combined.
  Delegation is done by having a token account that
  has a field that can point to another public key
  (which might delegate recursively!) to delegate voting control to.
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

## Sequencer election

Sequencer election works by doing an auction,
where the currency is ZEKO.

We do one auction per _Zeko_ epoch.
The length of a Zeko epoch is a constant number of slots,
and may not be aligned with L1 epochs.
However, it is probably a good idea to make it a constant factor
of L1 epochs and align it for ease of understanding.

The auction happens in the epoch 2 epochs
before the one for which it applies.
Thus, there is a space of 1 auction epoch where the results
are known to anyone.

For each slot in that epoch, the sequencer that can do a commit
in that slot is determined by the auction for that epoch.

In an auction, participants post (by posting an action to
the outer account), what they bid _per slot_,
and how many slots they would like to buy.
Slot preconditions enforce that it's done within the epoch.
Sequencers deposit their funds to a special account from which
they can withdraw the unused ZEKO.

The auction is _not_ private,
everyone can see what everyone else posts.
Making it private would be a non-trivial task.

Slot allocation could happen in many ways,
and it's not clear that there is an optimal best way.

Given the list of bids (many may exist for same public key),
iterate through the bids from highest slot rate to lowest,
and allocate from the beginning of the epoch,
starting with the highest bid, going for as many slots
as the bid buys, and then continuing until the next bid,
stopping when there are no more slots left.

Any unused ZEKO is refundible.

There should not be any unbid slots,
since for any sequencer it would make sense
to submit another bid for all slots for 0 ZEKO.

If however, this should still happen,
then anyone can sequence in this slot.
Effectively, it becomes a free-for-all.

## ZEKO token (unimplemented)

The ZEKO token will use the
[fungible token standard](https://github.com/MinaFoundation/mina-fungible-token),
which, interestingly, is also an implementation.
This is to enable third parties to use the token without importing foreign code
to generate the proof; every token uses the same vk, so you only need to use known
code.

We can however choose a vk that controls minting.
In our case, the vk delegates control the the outer account,
and the current policy of the outer account is to disallow minting
entirely.

The allocation is for now fixed, and determined when the token is deployed.

## Forced account update / govvernance (unimplemented)

(NB: permissions on account might be set to Either,
so this isn't necessarily the only way of doing a forced update).

If more than 2/3 vote yes on an account update (by signing it),
we permit it to pass.
We know the total amount of ZEKO,
and can prove how much ZEKO a public key owns by
setting a network precondition on `staking_epoch_data`,
from which we get the ledger used for the current epoch.
Then we can open the ledger and check how much ZEKO each account
owns.
Notably, we do not need to check _all_ accounts,
since we already know how much the total is.
We only need to calculate enough to get 2/3.

Since we can't expect everyone to be online,
we also allow delegation.
Delegation works by setting the public key
in a helper account.
Since we can't know if someone delegates
without checking this account,
you are forced to set this,
but you can set it to yourself.
Delegation can also be recursive,
such that the delegatee can delegate again.

Thus, given a signature on the account update,
to prove its weight, you start with accounts that
delegate to it, and then accounts that delegate to
those, and so on.

We however special-case the outer account of the rollup itself,
and instead consider the ledger inside the outer account
as a second ledger, used in the same way as above.

Of course, this assumes that you can transfer ZEKO.

## Our changes to transaction logic

We do not support processing failed transactions,
thus, you can not take fees from failed transactions.

Neither do we support any network preconditions currently.
We might support a subset in the future to allow checking the ledger hash.

We support time preconditions.
There is however not a single global slot when proving transactions.
There is a range, and that range (or a tighter one) must be used as precondition when committing.

The sequencer should however not accept transactions that have tight time bounds,
since the commit would be likely to fail, wasting work potentially.

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

## Emergency pause

There is a public key that can pause the rollup.
The public key is stored in the app state and can be changed via governance.

## Notable missing features

- Forcing a transaction to happen, bypassing the sequencer,
  to e.g. initiate and process a withdrawal.

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

## Pseudo-code spec

```ocaml
type ledger (* merkle tree *)

type commit = { ledger : ledger ; inner_action_state ; action_state ; slot_range : slot_range }
type deposit = { amount : nat ; recipient : PublicKey.t ; timeout : slot }
type withdrawal = { amount : nat ; recipient : PublicKey.t }
type match_ = { data : hash ; children : account_update_forest }

type outer_action =
  | Commit of commit
  | Bid of { rate : nat ; count : nat }
  | Match of match_
  | Time of { slot_range : slot_range } (* specialization of Match *)
  | Deposit of deposit

type inner_action =
  | Withdraw of withdrawal
  | Match of match_

type action_state

type outer_app_state =
  { ledger : ledger
  ; inner_action_state : action_state
  }

type inner_app_state =
  { outer_action_state : action_state
  }

type outer_helper_state =
  { next_withdrawal : nat
  ; next_cancelled_deposit : nat
  }

type inner_helper_state =
  { next_deposit : nat
  }

let do_inner_step ~source_action_state ~actionss =
  let target_action_state = List.fold ~init:source_action_state ~f:Actionss.push actionss
  [ { public_key = inner_pk
    ; app_state =
      { outer_action_state = target_action_state
      }
    ; preconditions =
      { outer_action_state = Some source_action_state
      }
    }
  ]

val max_slot_range_size : nat

let do_commit ~txn_snark ~slot_range ~new_actionss =
  assert slot_range.upper - slot_range.lower <= max_slot_range_size ;
  let old_inner = get_account inner_pk txn_snark.source in
  let new_inner = get_account inner_pk txn_snark.target in
  let action_state =
    (* We don't force sequencer to match on latest action state,
       since it's unreliable and might roll back. *)
    List.fold
      ~init:new_inner.app_state.outer_action_state
      ~f:Actionss.push new_actionss in
  let ledger = txn_snark.target in
  let inner_action_state = new_inner.action_state in
  [ { public_key = zeko_pk
    ; actions = [ Commit { ledger ; inner_action_state ; ~slot_range } ]
    ; app_state =
      { ledger
      ; inner_action_state
      }
    ; preconditions =
      { app_state =
        { ledger = Some txn_snark.source
        ; inner_action_state = Some old_inner.action_state
          (* technically unneeded, but added for good measure *)
        }
      ; valid_while = slot_range
      ; action_state
      }
    }
  ]

let do_time ~slot_range =
  [ { public_key = zeko_pk
    ; actions = [ Time { slot_range } ]
    ; preconditions =
      { valid_while = slot_range
      }
    }
  ]

let do_match_outer ~data ~children =
  [ { public_key = zeko_pk
    ; actions = [ Match { data ; children } ]
    ; preconditions =
      { children
      }
    }
  ]

let do_match_inner ~data ~children =
  [ { public_key = inner_pk
    ; actions = [ Match { data ; children } ]
    ; preconditions =
      { children
      }
    }
  ]

let do_deposit (deposit : deposit) =
  [ { public_key = zeko_pk
    ; actions = Deposit deposit
    ; balance_change = deposit.amount
    }
  ]

let do_withdraw ~withdrawal =
  [ { public_key = inner_pk
    ; actions = Withdraw withdrawal
    ; balance_change = withdrawal.amount
    }
  ]

let check_accepted ~timeout ~actions_after_deposit =
  let f acc action = match acc with
    | `Unknown -> match action with
      | Deposit _ -> `Unknown
      | Commit { slot_range ; _ } ->
        if slot_range.upper < timeout
          then `Accepted
          else `Unknown
      | Time { slot_range } ->
        if slot_range.lower > timeout
          then `Rejected
          else `Unknown
    | `Accepted -> `Accepted
    | `Rejected -> `Rejected
  List.fold ~init:`Unknown ~f actions_after_deposit

let do_finalize_deposit ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_deposit =
  assert check_accepted ~timeout:deposit.timeout ~actions_after_deposit = `Accepted ;
  assert prev_next_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_deposit
      ~actions:(Deposit deposit :: actions_after_deposit) in
  [ { public_key = inner_pk
    ; preconditions =
      { app_state =
        { outer_action_state
        }
      }
    ; balance_change = -deposit.amount
    ; children =
      [ { public_key = deposit.recipient
        ; use_full_commitment = true
          (* We shouldn't have to care about whether it's a proof
             or signature it's authorized by,
             but unfortunately there is no way to do a "full commitment"
             with a proof.
             There is a circular dependency, in that the recipient
             must confirm us, but we must also confirm
             that the recipient is confirming us. *)
        }
      ; { public_key = deposit.recipient
        ; token_id = Account_id.{ public_key = inner_pk, token_id = default }
        ; app_state =
          { next_deposit = action_state_before_deposit.length + 1
          }
        ; preconditions =
          { app_state =
            { next_deposit = Some prev_next_deposit
            }
          }
        }
      ]
    }
  ]

let do_finalize_cancelled_deposit ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_cancelled_deposit =
  assert check_accepted ~deposit ~actions_after_deposit = `Rejected ;
  assert prev_next_cancelled_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_deposit
      ~actions:(Deposit deposit :: actions_after_deposit) in
  [ { public_key = zeko_pk
    ; preconditions =
      { action_state = outer_action_state
      }
    ; balance_change = -deposit.amount
    ; children =
      [ { public_key = deposit.recipient
        ; use_full_commitment = true
        }
      ; { public_key = deposit.recipient
        ; token_id = Account_id.{ public_key = zeko_pk, token_id = default }
        ; app_state =
          { next_cancelled_deposit = action_state_before_deposit.length + 1
          }
        ; preconditions =
          { app_state =
            { next_cancelled_deposit = Some prev_next_cancelled_deposit
            }
          }
        }
      ]
    }
  ]

val withdrawal_delay : nat

let do_finalize_withdrawal ~withdrawal ~actions_after_withdrawal ~action_state_before_withdrawal ~prev_next_withdrawal ~commit ~actions_after_commit ~action_state_before_commit =
  assert prev_next_withdrawal <= action_state_before_withdrawal.length ;
  let inner_action_state =
    extend_action_state
      ~source:action_state_before_withdrawal
      ~actions:(Withdraw withdrawal :: actions_after_withdrawal) in
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_commit
      ~actions:(Commit commit :: actions_after_commit) in
  [ { public_key = zeko_pk
    ; preconditions =
      { app_state = { inner_action_state }
      ; valid_while = { lower = commit.slot_range.upper + withdrawal_delay ; upper = infinity }
      ; action_state = outer_action_state
      }
    ; balance_change = -deposit.amount
    ; children =
      [ { public_key = withdrawal.recipient
        ; use_full_commitment = true
        }
      ; { public_key = withdrawal.recipient
        ; token_id = Account_id.{ public_key = zeko_pk, token_id = default }
        ; app_state =
          { next_withdrawal = action_state_before_withdrawal.length + 1
          }
        ; preconditions =
          { app_state =
            { next_withdrawal = Some prev_next_withdrawal
            }
          }
        }
      ]
    }
  ]
```

## Pseudo-code example spec for token transfer smart contract

TODO: use fungible token standard contract

```ocaml
val public_key : PublicKey.t
val token_id : TokenId.t

type token_deposit = { amount : nat ; recipient : PublicKey.t ; timeout : slot }
type token_withdrawal = { amount : nat ; recipient : PublicKey.t }

type token_outer_helper_state =
  { next_withdrawal : nat
  ; next_cancelled_deposit : nat
  }

type token_inner_helper_state =
  { next_deposit : nat
  }

let token_deposit_action ~(sender : PublicKey.t) (deposit : token_deposit) : outer_action =
  let children =
    [ { account_id = token_id (* token owner *)
      ; children =
        [ { public_key = sender
          ; token_id
          ; balance_change = -deposit.amount
          ; may_use_token = Parents_own_token
          }
        ; { public_key
          ; token_id
          ; balance_change = deposit.amount
          ; may_use_token = Parents_own_token
          }
        ]
      }
    ]
  in
  Match { data = deposit ; children }


let do_token_deposit ~sender (deposit : token_deposit) =
  [ { public_key = zeko_pk
    ; actions = [ token_deposit_action ~sender deposit ]
    ; children
    }
  ]

let token_withdraw_action ~(sender : PublicKey.t) (withdrawal : token_withdrawal) : inner_action =
  let children =
    [ { public_key
      ; children =
        [ { public_key = sender
          ; token_id = AccountId.create public_key TokenId.default
          ; balance_change = -withdrawal.amount
          ; may_use_token = Parents_own_token
          }
      }
    ]
  in
  Match { data = withdrawal ; children }

let do_token_withdrawal ~sender (withdrawal : token_withdrawal) =
  [ { public_key = inner_pk
    ; actions = [ token_withdraw_action ~sender withdrawal ]
    ; children
    }
  ]

let do_finalize_token_deposit ~sender ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_deposit =
  assert check_accepted ~deposit ~actions_after_deposit = `Accepted ;
  assert prev_next_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_deposit
      ~actions:(token_deposit_action ~sender deposit :: actions_after_deposit) in
  [ { public_key
    ; children =
      [ { public_key = deposit.recipient
        ; token_id = AccountId.create public_key TokenId.default
        ; authorization_kind = Signature
        ; use_full_commitment = true
        ; balance_change = deposit.amount
        ; app_state =
          { next_deposit = action_state_before_deposit.length + 1
          }
        ; preconditions =
          { app_state =
            { next_deposit = Some prev_next_deposit
            }
          }
        }
      ; { public_key = inner_pk
        ; preconditions = { app_state = { outer_action_state } }
        }
      ]
    }
  ]

let do_finalize_cancelled_token_deposit ~sender ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_cancelled_deposit =
  assert check_accepted ~deposit ~actions_after_deposit = `Rejected ;
  assert prev_next_cancelled_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_deposit
      ~actions:(token_deposit_action ~sender deposit :: actions_after_deposit) in
  [ { account_id = token_id
    ; children =
      [ { public_key
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = -deposit.amount
        ; children =
          [ { public_key = zeko_pk
            ; preconditions = { action_state = outer_action_state }
            }
          ; { public_key = deposit.recipient
            ; token_id = Account_id.create public_key token_id
            ; may_use_token = Parents_own_token
            ; use_full_commitment = true
            ; authorization_kind = Signature
            ; app_state =
              { next_cancelled_deposit = action_state_before_deposit.length + 1
              }
            ; preconditions =
              { app_state =
                { next_cancelled_deposit = Some prev_next_cancelled_deposit
                }
              }
            }
          ]
        }
      ; { public_key = deposit.recipient
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = deposit.amount
        }
      ]
    }
  ]

val token_withdrawal_delay : nat

let do_finalize_token_withdrawal ~sender ~withdrawal ~actions_after_withdrawal ~action_state_before_withdrawal ~actions_after_commit ~action_state_before_commit ~commit ~prev_next_withdrawal =
  assert prev_next_withdrawal <= action_state_before_withdrawal.length ;
  let inner_action_state =
    extend_action_state
      ~source:action_state_before_withdrawal
      ~actions:(token_withdraw_action ~sender withdrawal :: actions_after_withdrawal) in
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_commit
      ~actions:(Commit commit :: actions_after_commit) in
  [ { account_id = token_id
    ; children =
      [ { public_key
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = -withdrawal.amount
        ; children =
          [ { public_key = zeko_pk
            ; preconditions =
              { action_state = outer_action_state
              ; app_state = { inner_action_state }
              ; valid_while =
                { lower = commit.slot_range.upper + token_withdrawal_delay
                ; upper = infinity }
              }
            }
          ; { public_key = withdrawal.recipient
            ; token_id = Account_id.create public_key token_id
            ; may_use_token = Parents_own_token
            ; use_full_commitment = true
            ; authorization_kind = Signature
            ; app_state =
              { next_withdrawal = action_state_before_withdrawal.length + 1
              }
            ; preconditions =
              { app_state =
                { next_withdrawal = Some prev_next_withdrawal
                }
              }
            }
          ]
        }
      ; { public_key = withdrawal.recipient
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = withdrawal.amount
        }
      ]
    }
  ]
```
