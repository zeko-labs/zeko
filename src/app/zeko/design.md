# Internal design of circuits

## Rough outline and motivation

We have one account for the rollup,
which we refer to as the _outer_ account.
The reason for this is that there is a corresponding
_inner_ account on the inside of the rollup,
which is a special account.
We have helper accounts per user on both sides,
they keep track of per-user deposits and withdrawals.
The inner accounts allows users on the inside to interact with
the rollup itself without extending account updates.

The outer account keeps track of three things:
- Ledger hash (Merkle tree of accounts / account db)
- Inner account action state (withdrawal requests)
- Deposit requests (as actions)
- Commits (as actions)

The inner account keeps track of
- Outer action state (deposit requests and commits)
- Withdrawal requests

The goal is to do "Mina on Mina", whereby we
can apply commands to the inner ledger (L2) and keep track of
it on the outside (L1) too.

The "keeping track" part is what we refer to as a _commit_,
i.e., we commit to the inner state on the L1.
The party that does the commit, i.e. submits the transaction upstream
is called the _sequencer_.

We call the act of transferring/briding MINA in as deposits,
and the opposite as withdrawals.
Though they seem symmetric, their behavior is slightly asymmetric.

Considerations:
- Deposits must have a timeout.
- Withdrawals must be delayed.
- Emergency pause must be possible.
- Emergency state change must be possible.
- Data availability should be maintained.
- User should mostly never lose fees when interacting on the outside.
- On the inside it is impossible to lose fees (we change zkapp command logic).
- We don't wish to support anything other than signed commands and zkapp commands.
- We don't wish to support account timing.
- We don't wish to support network preconditions.
- We _do_ wish to support time preconditions, to some extent.
- In the event that a commit is missed (e.g. rollback, censored by block producer),
  the amount of SNARK work wasted by sequencer should be minimal.
- We wish to support transfers/bridging of MINA only for now.

### Synchronization

First we must have a field that keeps track of the ledger hash on the outer account.
Whenever we commit, we must prove that a valid transition from the old hash to the new hash exists.
We must also synchronize the outer account and inner account,
transferring the information the inner account holds to the outer
account, and vice versa.
This is done by checking the state of the inner account in the outer account's
circuit and checking that it matches what it should be.
But the inner account may have been updated many times during one commit,
and to check that not just the latest value was correct,
the circuit for the inner account must check that each update was
also correct, such that correctness for each is implied from the final check
in the outer account's circuit.

The action state is also synchronised, such that the outer action state
is stored on the inner account, and the inner action state on the outer account.
But, we do only a partial synchronisation.
We post an old version of the outer action state to the inner account,
and verify this by proving that the old version is a predecessor of
the current action state.
This is done such that in the case that the action state on the L1 changes,
e.g. a rollback happens, we can still reuse most of our proofs (notably,
the proofs for the transition, since the outer action state is included in the
ledger, and thus affects all proofs).

However, this also means that naively, you can match on any arbitrary
previous action state, possibly _reverting_ the outer action state
as recorded on the inner account!
This is made impossible by the inner account's circuit,
since it only allows the outer action state contained to move forward.

This still isn't perfect:
What if sequencers refuse to move it forward?
There is after all no economic incentive for them.
Thus we change the behavior we expect from the sequencer:
The sequencer should synchronize the outer action state
to the inner account at the _beginning_ of the batch,
to maximize their own profits, by giving themselves the opportunity to reap
the fees of people finalizing their deposits.
They can choose to continually update it after that,
but at the end, they are still as always forced to synchronize
the latest inner action state to the outer account (but they are free
to block commands that add actions to the inner action state if they
wished to do so.)

### Deposits

Deposits are made by posting an action on the outer account,
along with sending the funds to the account (verified by proof).
The user specifies an upper bound (slot) after which point if not
processed, the deposit will timeout and the funds will be recoverible.

On commit, also post an action on the outer account that details
what kind of commit we made, along with the slot bounds.
We force the size of the slot bound to be no more than a constant,
such that the sequencer can't choose a range too wide.

On the inner side, the user can finalize a deposit that has been accepted.
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

In this process we must match on the outer action state as stored
in the inner account. This value can change, and cause the preconditions
to fail, but this is of no worry since failed transactions are feeless on Zeko.

### Withdrawals

To do withdrawals, we similarly post an action on the inner action state.
Unlike with deposits, there is only one type of action on the inner account.
On synchronization the inner account action state is stored on the outer account.

To withdraw, a constant number of slots must have roughly passed since the
withdrawal was added. To support this, in the "commit" actions, we also store
the inner action state as it was during that commit.
Given this, the user can prove an upper bound for when their withdrawal was added
by taking some commit that includes their action and using its upper slot bound.
There is however an issue here:
As with the deposit case, we must match on the inner action state stored on the
outer account's app state.
This might change.
Currently we don't work around this.
FIXME: fix #177.

As in the deposit case, we need to prevent double spends, thus similarly,
we have helper accounts on the outside too.
As with deposits, the helper account keeps track of the index of the last withdrawal
processed.
We also have to consider emergency changes.
The inner action state might "roll back" and procede in another direction
due to this, and this is why we store the inner action state in the outer
account explicitly instead of just as an action.
In addition to proving that it is contained in the inner action state stored
in the outer action state's "commit" action, we must prove that it's
also contained in the inner action state recorded in the outer app state.

### Cancelled deposits

There is however one more kind of transfer:
Cancelled deposits.
A cancelled deposit can be finalized analogously to deposits,
but on the outer side, by proving that the action corresponding to the
deposit has been followed by an "time" action, which lower slot bound
exceeds the timeout slot, while no "commit" action which upper bound is less than
the timeout slot precedes it but comes after the deposit's action.

Analogously, to prevent double spends, we must keep track of this.
We use the same helper account as for withdrawals,
thus the helper account on the outside keeps track of two indices,
one for the index of the last withdrawal processed, and
one for the index of the last cancelled deposit processed.

### Time/network preconditions

We don't support any network preconditions,
but we do support `valid_while`.
The transaction snark circuit keeps track of the most conservative
`valid_while` that would satisfy all transactions included in the
transition, i.e. time preconditions on the inside are translated
to time preconditions on the outside.
Thus, time on the L2 tracks time on the L1 in some sense.

The sequencer should take care not to admit time preconditions
that are too risky, since any SNARK work done after that precondition
will be invalidated if the precondition fails on the L1 (for example,
due to censorship by the block producer chosen for that slot).

### ZEKO token (unimplemented)

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

### Forced account update (unimplemented)

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
in the _helper_ account.
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

### Sequencer election

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

## Transferring custom information

We can transfer information too.
We support a type of action on the outer accounts
that includes the hash of its children in the action,
such that you can witness arbitrary information about the L1.

You can implement custom token transfers this way,
by creating a two accounts, one on the inside, and one
on the outside.

The inside one is a token owner and mints
the token when you can prove that you've deposit the token on the outside
to the outer special account.

You can implement timeouts too, using the same "time" action,
and withdrawals can be delayed the same way.

This will be used to transfer ZEKO.
This will not replace the native transfer mechanism for MINA,
since MINA is special-cased.
Contrary to the other ones,
MINA doesn't have a token owner, and thus can't be minted.
Neither does it use the fungible token standard.

## Notable missing features

- Force withdrawing funds, bypassing sequencer (seems hard?)

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

let do_finalize_token_withdrawal ~sender ~withdrawal ~actions_after_withdrawal ~action_state_before_withdrawal ~actions_after_commit ~action_state_before_commit ~prev_next_withdrawal =
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
