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
Though they seem symmetric, their behaviour is slightly asymmetric.

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
This is obviously suboptimal.
Thus we check that the length of the action state (# actions) exceeds
the old one, but this still isn't perfect:
What if sequencers refuse to move it forward?
There is after all no economic incentive for them.
Thus we change the behaviour we expect from the sequencer:
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

## Notable missing features

- Sequencer election
- Force withdrawing funds, bypassing sequencer (seems hard?)

## Pseudo-code spec

```ocaml
type action_state
type action_state_with_length
type ledger (* merkle tree *)

type deposit = { amount : nat ; recipient : PublicKey.t ; timeout : slot }
type commit = { inner_action_state : action_state ; slot_range : slot range }

type outer_action =
  | Deposit of deposit
  | Commit of commit
  | Time of { slot_range : slot_range }

type withdrawal = { amount : nat ; recipient : PublicKey.t }
type inner_action =
  | Withdraw of withdrawal

type outer_app_state =
  { inner_action_state : action_state
   (** Can technically be deduced from ledger_hash,
       yet we still include it to lessen the proving load
       users have to do on withdrawal. *)

  ; ledger : ledger
  }

type inner_app_state =
  { outer_action_state : action_state_with_length
  }

type outer_helper_app_state =
  { last_withdrawal : nat
  ; last_cancelled_deposit : nat
  }

type inner_helper_app_state =
  { last_deposit : nat
  }

type action_state_extension

val action_state_extension_source :
  action_state_extension ->
  action_state_with_length

val action_state_extension_target :
  action_state_extension ->
  action_state_with_length

let do_inner_step ~action_state_extension =
  [ { public_key = inner_pk
    ; app_state =
      { outer_action_state =
        action_state_extension_source action_state_extension
      }
    ; preconditions =
      { outer_action_state =
        Some (action_state_extension_target action_state_extension)
      }
    }
  ]

let do_commit ~txn_snark ~action_state_extension ~slot_range =
  let new_inner = get_account inner_pk txn_snark.target in
  let old_inner = get_account inner_pk txn_snark.source in
  assert new_inner.app_state.outer_action_state = action_state_extension.target ;
  [ { public_key = zeko_pk
    ; actions = Commit { inner_action_state ; ~slot_range }
    ; app_state =
      { ledger_hash = txn_snark.target
      ; inner_action_state = new_inner.action_state
      }
    ; preconditions =
      { app_state =
        { ledger_hash = Some txn_snark.source
        ; inner_action_state = None
        }
      ; action_state = action_state_extension.source
      ; valid_while = slot_range
      }
    }
  ]

let do_deposit (deposit : deposit) =
  [ { public_key = zeko_pk
    ; actions = Deposit deposit
    ; balance_change = amount
    }
  ]

let do_time ~slot_range =
  [ { public_key = zeko_pk
    ; actions = Time { slot_range }
    ; preconditions =
      { valid_while = slot_range
      }
    }
  ]

let do_withdraw ~amount ~recipient =
  [ { public_key = inner_pk
    ; actions = Withdraw { amount ; recipient }
    ; balance_change = amount
    }
  ]

type accepted_deposit

let check_accepted ~deposit ~actions_after_deposit =
  let f acc action = match acc with
    | `Unknown -> match action with
      | Deposit _ -> `Unknown
      | Commit { slot_range ; _ } ->
        if slot_range.upper < deposit.timeout
          then `Accepted
          else `Unknown
      | Time { slot_range } ->
        if slot_range.lower > deposit.timeout
          then `Rejected
          else `Unknown
    | `Accepted -> `Accepted
    | `Rejected -> `Rejected
  List.fold ~init:`Unknown ~f actions_after_deposit

let do_finalize_deposit ~deposit ~actions_after_deposit ~action_state_before_deposit ~last_deposit_index =
  assert check_accepted ~deposit ~actions_after_deposit = `Accepted ;
  assert last_deposit_index < action_state_before_deposit.length ;
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_deposit
      ~actions:(deposit :: actions_after_deposit) in
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
          { last_deposit = action_state_before_deposit.length
          }
        ; preconditions =
          { app_state =
            { last_deposit = Some last_deposit_index
            }
          }
        }
      ]
    }
  ]

let do_finalize_cancelled_deposit ~deposit ~actions_after_deposit ~action_state_before_deposit ~last_cancelled_deposit_index =
  assert check_accepted ~deposit ~actions_after_deposit = `Rejected ;
  assert last_cancelled_deposit_index < action_state_before_deposit.length ;
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
          { last_cancelled_deposit = action_state_before_deposit.length
          }
        ; preconditions =
          { app_state =
            { last_cancelled_deposit = Some last_cancelled_deposit_index
            }
          }
        }
      ]
    }
  ]

val withdrawal_delay : nat

let do_finalize_withdrawal ~withdrawal ~actions_after_withdrawal ~action_state_before_withdrawal ~last_withdrawal_index ~commit ~actions_after_commit ~action_state_before_commit =
  assert last_withdrawal_index < action_state_before_withdrawal.length ;
  let inner_action_state =
    extend_action_state
      ~source:action_state_before_withdrawal
      ~actions:(Withdraw withdrawal :: actions_after_withdrawal) in
  let outer_action_state =
    extend_action_state
      ~source:action_state_before_commit
      ~actions:(Commit commit :: actions_after_withdrawal) in
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
          { last_withdrawal = action_state_before_withdrawal.length
          }
        ; preconditions =
          { app_state =
            { last_withdrawal = Some last_withdrawal_index
            }
          }
        }
      ]
    }
  ]
```
