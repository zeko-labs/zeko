# Pseudo-code example spec for token bridge smart contract

The zkApp on L1 acts as a bank for the token in question,
emitting a note essentially that allows you to withdraw from
a corresponding bank on the L2.
The bank on the L2 must be seeded with the appropriate amount of the token.

Account updates that have authorization_kind = Proof,
where the account id is `holder_account_l1`, or where the token id is `helper_token_id_l1`,
define the circuit for the verification keys for the L1-side of the bridge contract.
Likewise, the ones for `account_id_l2` define the circuit for the L2-side.

Things to consider:
- State of rollup can arbitrarily change potentially through governance.
- How much historical data do you need to prove a deposit?

There are multiple accounts on the L1 that correspond to the single account on the L2,
such that in the event of a hack (vulnerability is found in circuit), only
~one/two of the L1 banks can get their funds stolen.

This is done via disabling and enabling accounts at certain intervals.
Disabled accounts are switched to an entirely different circuit,
which only function is enabling the account again.

When disabled, the `send` permission is set to `Impossible`.

The expectation is that some good samaritan will take on the task
of doing this switching, albeit if there is more than such good samaritan, the one who
doesn't succeed will needlessly pay transaction fees.
This is deemed to be an acceptable cost.

NB: We don't require that user actions are only done in the enabled period.
This wouldn't improve security, and would increase circuit size and complexity.
OTOH, users should not use an L1 account/bank which is soon to become disabled.

```ocaml
val helper_token_owner_l1 : Public_key.t
val public_key_l2 : Public_key.t
val token_id_l1 : Token_id.t
val token_id_l2 : Token_id.t
val holder_accounts_l1 : Public_key.t list
val window_size : nat
let helper_token_id_l1 = Account_id.create helper_token_owner_l1 Token_id.default

let account_id_l2 = Account_id.create public_key_l2 token_id_l2

type deposit = { amount : nat ; recipient : Public_key.t ; timeout : slot }
type withdrawal = { amount : nat ; recipient : Public_key.t }

type outer_state =
  { disable_offset_lower : nat
  ; disable_offset_upper : nat
  ; disable_period : nat
  ; enable_offset_lower : nat
  ; enable_offset_upper : nat
  ; enable_period : nat
  ; disabled_vk : vk
  ; enabled_vk : vk
  } (* maybe should be ints? *)

type outer_helper_state =
  { next_withdrawal : nat
  ; next_cancelled_deposit : nat
  }

type inner_helper_state =
  { next_deposit : nat
  }

type deposit_params =
  { authorization_kind : Authorization_kind.t
  ; nested_children : call_forest
  ; call_data : 'a
  ; children : call_forest
  ; deposit : deposit
  ; holder_account_l1
  ; authorization_kind
  }

(* L1 *)
let deposit_action (params : deposit_params) : outer_action =
  assert List.mem params.holder_account_l1 holder_accounts_l1 ;
  let a =
    { public_key = params.holder_account_l1
    ; token_id = token_id_l1
    ; balance_change = params.deposit.amount
    ; may_use_token = Parents_own_token
    ; authorization_kind = params.authorization_kind
    }
  in
  let a' =
    if token_id_l1 == Token_id.default
    then a
    else
      { account_id = token_id_l1 (* token owner *)
      ; call_data = params.call_data
      ; authorization_kind = params.authorization_kind
      ; children = a :: params.nested_children
      }
  in
  let children = a' :: params.children in
  Witness { aux = params.deposit ; children ; valid_while = infinite_valid_while }

type withdrawal_params =
  { authorization_kind : Authorization_kind.t
  ; nested_children : call_forest
  ; call_data : 'a
  ; children : call_forest
  ; withdrawal : withdrawal
  }

(* L2 *)
let withdraw_action
  (params : withdrawal_params) : inner_action =
  let a =
    { public_key = public_key_l2
    ; token_id = token_id_l2
    ; balance_change = params.withdrawal.amount
    ; may_use_token = Parents_own_token
    ; authorization_kind = None
    }
  in
  let a' =
    if token_id == Token_id.default
    then a
    else
      { account_id = token_id_l2 (* token owner *)
      ; call_data = params.call_data
      ; authorization_kind = params.authorization_kind
      ; children = a :: params.nested_children
      }
  in
  let children = a' :: params.children in
  Witness { aux = params.withdrawal ; children }


let check_accepted ~deposit ~actions_after_deposit ~deposit_index =
  let f = function
    | `Unknown -> begin function
      | Commit { valid_while ; synchronized_outer_action_state_length ; _ } ->
        if valid_while.lower > deposit.timeout then `Rejected
        else if
          valid_while.upper < deposit.timeout &&
          synchronized_outer_action_state_length > deposit_index
        then `Accepted
        else `Unknown
      | Witness { valid_while ; _ } ->
        if valid_while.lower > deposit.timeout then `Rejected else `Unknown
    end
    | `Rejected -> fun _ -> `Rejected
    | `Accepted -> fun _ -> `Accepted
  in
  List.fold_right ~init:`Unknown ~f actions_after_deposit

(* L2, necessary because access = Proof *)
let do_inner_receive =
  assert amount > 0 ;
  { account_id = account_id_l2
  ; balance_change = amount
  ; authorization_kind = Proof
  ; may_use_token = Parents_own_token
  }

(* L2 *)
let do_finalize_deposit
  ~deposit_params
  ~actions_after_deposit
  ~action_state_before_deposit
  ~prev_next_deposit
  ~may_use_token
  ~inner_authorization_kind
  ~deposit_index
  =
  let deposit = deposit_params.deposit in
  assert check_accepted ~deposit ~actions_after_deposit ~deposit_index = `Accepted ;
  (* the index of the deposit we're finalizing must be higher or
     equal to the index of the lowest possible deposit we can finalize *)
  assert prev_next_deposit <= deposit_index ;
  let outer_action_state =
    List.append actions_after_deposit (deposit_action deposit_params :: action_state_before_deposit)
  in
  let outer_action_state_length =
    List.length actions_after_deposit + 1 + deposit_index
  in
  { account_id = account_id_l2
  ; balance_change = -deposit.amount
  ; may_use_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = deposit.recipient
      ; token_id = account_id_l2
      ; authorization_kind = Signature
      ; use_full_commitment = true
      ; may_use_token = Parents_own_token
      ; app_state =
        { next_deposit = deposit_index + 1
        }
      ; preconditions =
        { app_state =
          { next_deposit = prev_next_deposit
          }
        }
      }
    ; { public_key = inner_pk
      ; preconditions = { app_state = { outer_action_state ; outer_action_state_length } }
      ; authorization_kind = inner_authorization_kind
      }
    ]
  }

(* L1 *)
(*
  Prove that we have submitted a deposit, and that it's been rejected.
  We do this by showing that there is a historical action state after
  which our deposit comes, and after which there are actions that reject it
  before accepting it.
  
  We then update the stored next_cancelled_deposit index as done in the other cases.
  
  The challenge lies in that we need to prove that the deposit index is correct,
  since we're counting from the beginning, and not from the end.
  We need to know that there indeed was that many actions before our deposit,
  but we would like to prevent having to count back to the dawn of our rollup.
  
  We instead find a commit in the past to use its synchronization point to skip
  most of the history.
  We count from the synchronization point to the current outer action state to
  calculate the current length of the outer action state.
  We use that to ensure that our deposit index is correct.
*)
let do_finalize_cancelled_deposit
  ~deposit_params
  ~actions_after_deposit
  ~action_state_before_deposit
  ~prev_next_cancelled_deposit
  ~outer_authorization_kind
  ~deposit_index
  ~may_use_token
  ~commit
  ~action_state_before_commit
  ~actions_after_commit
  ~actions_after_synchronization
  =
  let deposit = deposit_params.deposit in
  assert check_accepted ~deposit ~actions_after_deposit ~deposit_index = `Rejected ;
  assert prev_next_cancelled_deposit <= deposit_index ;
  let outer_action_state =
    List.append actions_after_deposit (deposit_action deposit_params :: action_state_before_deposit)
  in
  let outer_action_state_length =
    List.length actions_after_deposit + 1 + deposit_index
  in
  let outer_action_state' =
    List.append actions_after_commit (Commit commit :: action_state_before_commit)
  in
  assert outer_action_state' = outer_action_state ;
  let outer_action_state =
    List.append actions_after_synchronization commit.synchronized_outer_action_state
  in
  assert outer_action_state'' = outer_action_state ;
  let outer_action_state_length' =
    List.length actions_after_synchronization + commit.synchronized_outer_action_state_length
  in
  assert outer_action_state_length = outer_action_state_length' ;
  { account_id = deposit_params.holder_account_l1
  ; balance_change = -deposit.amount
  ; may_use_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = helper_token_owner_l1
      ; authorization_kind = Proof
      ; children =
        [ { public_key = deposit.recipient
          ; token_id = helper_token_id_l1
          ; authorization_kind = Signature
          ; use_full_commitment = true
          ; may_use_token = Parents_own_token
          ; app_state =
            { next_cancelled_deposit = deposit_index + 1
            }
          ; preconditions =
            { app_state =
              { next_cancelled_deposit = prev_next_deposit
              }
            }
          }
        ]
      }
    ; { public_key = zeko_pk
      ; preconditions =
        { action_state = outer_action_state
        ; app_state =
          { paused = false
          }
        }
      ; authorization_kind = outer_authorization_kind
      }
    ]
  }

(* L1 *)
(* This does the same as the above but uses different information.
   The above doesn't need all actions in existence, but this does,
   in exchange for not needing to depend on the existence of a Commit action.
*)
let do_finalize_cancelled_deposit_simple
  ~deposit_params
  ~actions_after_deposit
  ~prev_next_cancelled_deposit
  ~outer_authorization_kind
  ~may_use_token
  =
  let deposit = deposit_params.deposit in
  let deposit_index = List.length action_state_before_deposit in
  assert check_accepted ~deposit ~actions_after_deposit ~deposit_index = `Rejected ;
  assert prev_next_cancelled_deposit <= deposit_index ;
  let outer_action_state =
    List.append actions_after_deposit (deposit_action deposit_params :: action_state_before_deposit)
  in
  { account_id = deposit_params.holder_account_l1
  ; balance_change = -deposit.amount
  ; may_use_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = helper_token_owner_l1
      ; authorization_kind = Proof
      ; children =
        [ { public_key = deposit.recipient
          ; token_id = helper_token_id_l1
          ; authorization_kind = Signature
          ; use_full_commitment = true
          ; may_use_token = Parents_own_token
          ; app_state =
            { next_cancelled_deposit = deposit_index + 1
            }
          ; preconditions =
            { app_state =
              { next_cancelled_deposit = prev_next_deposit
              }
            }
          }
        ]
      }
    ; { public_key = zeko_pk
      ; preconditions =
        { action_state = outer_action_state
        ; app_state =
          { paused = false
          }
        }
      ; authorization_kind = outer_authorization_kind
      }
    ]
  }

val withdrawal_delay : nat

(* L1 *)
let do_finalize_withdrawal
  ~withdrawal_params
  ~actions_after_withdrawal
  ~action_state_before_withdrawal
  ~actions_after_commit
  ~action_state_before_commit
  ~commit
  ~prev_next_withdrawal
  ~withdrawal_index
  ~outer_authorization_kind
  ~may_use_token
  =
  let withdrawal = withdrawal_params.withdrawal in
  assert prev_next_withdrawal <= withdrawal_index ;
  let inner_action_state =
    List.append actions_after_withdrawal @@ withdraw_action withdrawal_params :: action_state_before_withdrawal
  in
  let outer_action_state =
    List.append actions_after_commit @@ Commit commit :: action_state_before_commit
  in
  let inner_action_state_length = List.length actions_after_withdrawal + 1 + withdrawal_index in
  (* FIXME: It isn't checked whether inner_action_state is still valid, it's only inferred
     that it is from the Commit action.
     This is safe unless the inner_action_state is forcibly changed, after which point
     you'd also have to fix the circuit to check that the inner_action_state is
     still valid or do some other kind of thing to prevent the invalid actions from being used.
  *)
  assert commit.inner_action_state = inner_action_state ;
  assert commit.inner_action_state_length = inner_action_state_length ;
  { account_id = holder_account_l1
  ; balance_change = -withdrawal.amount
  ; may_use_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = helper_token_owner_l1
      ; authorization_kind = Proof
      ; children =
        [ { public_key = withdrawal.recipient
          ; token_id = helper_token_id_l1
          ; authorization_kind = Signature
          ; use_full_commitment = true
          ; may_use_token = Parents_own_token
          ; app_state =
            { next_withdrawal = withdrawal_index + 1
            }
         ; preconditions =
            { app_state =
              { next_withdrawal = prev_next_withdrawal
              }
            }
          }
        ]
      }
    ; { public_key = zeko_pk
      ; authorization_kind = outer_authorization_kind
      ; preconditions =
        { action_state = outer_action_state
        ; app_state = { paused = false }
        ; valid_while =
          { lower = commit.valid_while.upper + withdrawal_delay
          ; upper = infinity }
        }
      }
    ]
  }

let do_disable ~disabled_vk ~disable_offset_lower ~disable_offset_upper ~disable_period idx =
  { account_id = holder_accounts_l1.(account_idx)
  ; set_permissions =
    { send = Impossible
    }
  ; set_vk = disabled_vk
  ; preconditions =
    { valid_while =
      { lower = disable_offset_lower + idx * disable_period
      ; upper = disable_offset_upper + idx * disable_period
      }
      (*
      { lower = (account_idx + 1 + idx * List.length holder_accounts_l1) * window_size
      ; upper = (account_idx + (1 + idx) * List.length holder_accounts_l1) * window_size - 1
      }
      *)
    ; app_state =
      { disable_offset_upper
      ; disable_offset_lower
      ; disable_period
      ; disabled_vk
      }
    }
  }
```

Circuit when disabled
```ocaml
let do_enable ~enabled_vk ~enable_offset_lower ~enable_offset_upper ~enable_period idx =
  { account_id = holder_accounts_l1.(account_idx)
  ; set_permissions =
    { send = Impossible
    }
  ; set_vk = enabled_vk
  ; preconditions =
    { valid_while =
      { lower = enable_offset_lower + idx * enable_period
      ; upper = enable_offset_upper + idx * enable_period
      }
      (*
      { lower = (account_idx + 1 + idx * List.length holder_accounts_l1) * window_size
      ; upper = (account_idx + (1 + idx) * List.length holder_accounts_l1) * window_size - 1
      }
      *)
    ; app_state =
      { enable_offset_upper
      ; enable_offset_lower
      ; enable_period
      ; enabled_vk
      }
    }
  }
```

Init state
```ocaml
let init_inner =
  { Account.empty with
  ; account_id = account_id_l2
  ; balance = Currency.Amount.max
  ; permissions = all_proof
  }

let init_outer =
  { Account.empty with
  ; account_id = account_id_l1
  ; app_state =
    { disable_offset_lower = (* figure out *)
    ; disable_offset_upper = (* figure out *)
    ; disable_period = (* figure out *)
    ; enable_offset_lower = (* figure out *)
    ; enable_offset_upper = (* figure out *)
    ; enable_period = (* figure out *)
    ; disabled_vk
    ; enabled_vk
    }
  ; permissions = { all_proof with access = None ; receive = None }
  }

let init_outer_token_owner =
  { Account.empty with
  ; public_key = helper_token_owner_l1
  ; permissions = all_proof
  }
```

## Avoiding failing precondition on withdrawal

We have a precondition on the `inner_app_state` in the rollup zkapp
on withdrawals, which can obviously fail.
We can cooperate with the sequencer and jointly construct a transaction
such that a payment to the sequencer is included.
The sequencer is then incentivized to align their commits such that this
withdrawal transaction succeeds, otherwise they wouldn't get their fee.

## Proving considerations

Sequencer should save proofs of action state extensions proved
and also make them queriable such that users don't have to prove
more than they need to.
Users can take these and merge them to prove the extension from their
deposit/withdrawal until the synchronized point.

check_accepted should not be used as much as possible because the proof
is specific to the deposit in question rather than being generic.
