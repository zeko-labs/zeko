# Pseudo-code example spec for token bridge smart contract

The zkApp on L1 acts as a bank for the token in question,
emitting a note essentially that allows you to withdraw from
a corresponding bank on the L2.
The bank on the L2 must be seeded with the appropriate amount of the token.

Account updates that have authorization_kind = Proof,
where the account id is `holder_account_l1`, or where the token id is `helper_token_id`,
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
let helper_token_id = Account_id.create helper_token_owner_l1 Token_id.default

let account_id_l2 = Account_id.create public_key_l2 token_id_l2

type deposit = { amount : nat ; recipient : Public_key.t ; timeout : slot }
type withdrawal = { amount : nat ; recipient : Public_key.t }

type outer_state =
  {
  }

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
  }

(* L1 *)
let deposit_action (params : deposit_params) : outer_action =
  let a =
    { public_key = params.holder_account_l1
    ; token_id = token_id_l1
    ; balance_change = params.deposit.amount
    ; may_use_token = Parents_own_token
    ; authorization_kind = None
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
    ; balance_change = withdrawal.amount
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


let check_accepted ~deposit ~actions_after_deposit =
  let f = function
    | `Unknown -> begin function
      | Commit { valid_while ; _ } ->
        (* FIXME: incorrect, check synchronized_outer_action_state *)
        if valid_while.lower > deposit.timeout then `Rejected
        else if valid_while.upper < deposit.timeout then `Accepted
        else `Unknown
      | Witness { valid_while ; _ } ->
        if valid_while.lower > deposit.timeout then `Rejected else `Unknown
    end
    | `Rejected -> fun _ -> `Rejected
    | `Accepted -> fun _ -> `Accepted
  in
  List.fold_right ~init:`Unknown ~f actions_after_deposit

(* L2 *)
let do_finalize_deposit
  ~deposit_params
  ~actions_after_deposit
  ~action_state_before_deposit
  ~prev_next_deposit
  ~may_use_token
  ~inner_authorization_kind
  =
  let deposit = deposit_params.deposit in
  assert check_accepted ~deposit ~actions_after_deposit = `Accepted ;
  (* the index of the deposit we're finalizing must be higher or
     equal to the index of the lowest possible deposit we can finalize *)
  assert prev_next_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    actions_after_deposit ++ deposit_action deposit_params :: action_state_before_deposit
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
        { next_deposit = action_state_before_deposit.length + 1
        }
      ; preconditions =
        { app_state =
          { next_deposit = prev_next_deposit
          }
        }
      }
    ; { public_key = inner_pk
      ; preconditions = { app_state = { outer_action_state } }
      ; authorization_kind = inner_authorization_kind
      }
    ]
  }

(* L1 *)
let do_finalize_cancelled_deposit
  ~deposit_params
  ~actions_after_deposit
  ~action_state_before_deposit
  ~prev_next_cancelled_deposit
  ~holder_account_l1
  =
  let deposit = deposit_params.deposit in
  assert check_accepted ~deposit ~actions_after_deposit = `Rejected ;
  assert prev_next_cancelled_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    actions_after_deposit :: deposit_action deposit_params ++ action_state_before_deposit
  in
  { account_id = holder_account_l1
  ; balance_change = -deposit.amount
  ; may_use_token = Parents_own_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = helper_token_owner_l1
      ; authorization_kind = Proof
      ; children =
        [ { public_key = deposit.recipient
          ; token_id = helper_token_id
          ; authorization_kind = Signature
          ; use_full_commitment = true
          ; may_use_token = Parents_own_token
          ; app_state =
            { next_cancelled_deposit = action_state_before_deposit.length + 1
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
      ; authorization_kind = None
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
  =
  let withdrawal = withdrawal_params.withdrawal in
  assert prev_next_withdrawal <= action_state_before_withdrawal.length ;
  let inner_action_state =
    actions_after_withdrawal ++ withdraw_action withdrawal_params :: action_state_before_withdrawal
  in
  let outer_action_state =
    actions_after_commit ++ Commit commit :: action_state_before_commit
  in
  assert commit.inner_action_state = inner_action_state ;
  { account_id = holder_account_l1
  ; balance_change = -withdrawal.amount
  ; may_use_token = Parents_own_token
  ; authorization_kind = Proof
  ; children =
    [ { public_key = helper_token_owner_l1
      ; authorization_kind = Proof
      ; children =
        [ { public_key = withdrawal.recipient
          ; token_id = helper_token_id
          ; authorization_kind = Signature
          ; use_full_commitment = true
          ; may_use_token = Parents_own_token
          ; app_state =
            { next_withdrawal = action_state_before_withdrawal.length + 1
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
      ; authorization_kind = None
      ; preconditions =
        { action_state = outer_action_state
        ; app_state = { inner_action_state ; paused = false }
        ; valid_while =
          { lower = commit.valid_while.upper + withdrawal_delay
          ; upper = infinity }
        }
      }
    ]
  }

let get_valid_while_for_disable outer_state idx =
  { lower = outer_state.disable_offset_lower + idx * outer_state.disable_period
  ; upper = outer_state.disable_offset_upper + idx * outer_state.disable_period
  }
  (*
  { lower = (account_idx + 1 + idx * List.length holder_accounts_l1) * window_size
  ; upper = (account_idx + (1 + idx) * List.length holder_accounts_l1) * window_size - 1
  }
  *)

let do_disable account_idx idx =
  { account_id = holder_accounts_l1.(account_idx)
  ; set_permissions =
    { send = Impossible
    }
  ; set_vk = <vk for next circuit>
  ; preconditions =
    { valid_while = get_valid_while_for_disable account_idx idx
    }
  }
```

Circuit when disabled
```ocaml
let get_valid_while_for_enable outer_state idx =
  { lower = outer_state.enable_offset_lower + idx * outer_state.enable_period
  ; upper = outer_state.enable_offset_upper + idx * outer_state.enable_period
  }
  (*
  { lower = (account_idx + idx * List.length holder_accounts_l1) * window_size
  ; upper = (account_idx + idx * List.length holder_accounts_l1 + 1) * window_size - 1
  }
  *)

let do_enable =
  { account_id = holder_account_l1
  ; set_permissions =
    { send = Proof
    }
  ; set_vk = <vk for previous circuit>
  ; preconditions =
    { valid_while = get_valid_while_for_enable account_idx idx
    }
  }
  }
```

## Avoiding failing precondition on withdrawal

We have a precondition on the `inner_app_state` in the rollup zkapp
on withdrawals, which can obviously fail.
We can cooperate with the sequencer and jointly construct a transaction
such that a payment to the sequencer is included.
The sequencer is then incentivized to align their commits such that this
withdrawal transaction succeeds, otherwise they wouldn't get their fee.
