# Core rollup spec with sequencer auction

```ocaml
type ledger

val macroslot_size : nat (* probably around 1 day *)
val macroslot_switch_boundary : nat (* must be less than size, e.g. around 50% of it *)

type macroslot = nat

type commit =
  { ledger : ledger
  ; inner_action_state : action_state
  ; synchronized_outer_action_state : action_state
  ; sequencer : sequencer
  ; macroslot : macroslot
  ; valid_while : valid_while
  }

and type outer_action =
  | Commit of commit
  | Bid of { sequencer : public_key ; rate : nat ; start : nat ; stop : nat ; valid_while : valid_while }
  | Witness of { aux : 'a ; children : account_update_forest ; valid_while : valid_while }

and type outer_action_state = outer_action list

type inner_action =
  | Witness of { aux : 'a ; children : account_update_forest }

type inner_action_state = inner_action list

type outer_app_state =
  { ledger : ledger
  ; inner_action_state : inner_action_state
  ; sequencer : public_key
  ; macroslot : macroslot
  ; bid_rate : nat
  ; finalized : bool
  }

type inner_app_state =
  { outer_action_state : outer_action_state
  }

(* happens inside rollup, on L2 *)
let do_inner_step ~source_action_state ~actions =
  [ { public_key = inner_pk
    ; app_state =
      { outer_action_state = List.append actions source_action_state
      }
    ; preconditions =
      { outer_action_state = source_action_state
      }
    }
  ]

val max_valid_while_size : nat

val bid_min_time_before : nat (* probably 1 day *)
val bid_max_time_before : nat (* probably 2 days *)

val bid_deposit_factor : nat

val zeko_token_owner : account_id

let do_bid ~rate ~start ~stop ~valid_while =
  assert start - valid_while.upper > bid_min_time_before ;
  assert stop - valid_while.lower < bid_max_time_before ;
  [ { public_key = zeko_pk
    ; actions = Bid { rate ; start ; stop ; valid_while }
    ; preconditions = { valid_while }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = zeko_pk
            ; balance_change = rate
            }
          ]
        }
      ]
    }
  ]

let macroslot_lower macroslot = macroslot * macroslot_size

let get_winning_bid
  ~action_state_before_bid
  ~time_action
  ~actions_for_bid
  ~macroslot =
  (* bid logic *)
  let action_state = (actions_for_bid ++ (time_action :: action_state_before_bid)) in

  (* We only consider bids that have been made after some slot,
     time_action is an action before the bids begin that
     has a time bound before the bids are valid. *)
  (match time_action with
    | Witness { valid_while ; _ } -> assert false
    | Bid { valid_while ; _ } -> valid_while.upper < macroslot_lower macroslot - bid_max_time_before
    | Commit { valid_while ; _ } -> valid_while.upper < macroslot_lower macroslot - bid_max_time_before
  )

  let Some (sequencer, rate) = List.fold actions_for_bid ~init:None ~f:(fun acc -> function
    | Bid { sequencer ; rate ; start ; stop ; _ } ->
      if start <= macroslot & stop > macroslot
        then (match acc with
          None -> (sequencer, rate)
          Some (sequencer', rate') ->
            if rate > rate'
              then (sequencer, rate)
              else (sequencer', rate')
        )
        else acc
    | _ -> acc
  ) in
  (action_state, sequencer, rate)


(* The case where we switch the sequencer peacefully,
   the previous sequencer has finalized, but we assume
   that it's stable, and continue working from that point.
*)
let do_switch_sequencer
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~previous_sequencer
  ~previous_bid_rate
  ~ledger
  =
  let (action_state, sequencer, bid_rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = previous_bid_rate
        ; finalized = true
        }
      ; action_state
      ; valid_while =
        { lower = macroslot_lower (macroslot - 1) + macroslot_switch_boundary + 1
          (* We allow using this in the previous macroslot too,
             since if they've finalized their turn we can begin
             ours early. *)
        ; upper = macroslot_lower macroslot + macroslot_switch_boundary
        }
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -bid_rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              bid_rate * bid_deposit_factor
              -
              previous_bid_rate * (bid_deposit_factor - 1)
            }
          ; { token_id = zeko_token_owner
            ; public_key = previous_sequencer
            ; amount = previous_bid_rate * (bid_deposit_factor - 1)
            }
          ]
        }
      ]
    }
  ]


(* The case where we switch the sequencer,
   but the previous sequencer has not finalized their turn.
   NB: We must make sure that we are exactly in our macroslot,
   unlike the other cases.
*)
let do_switch_sequencer_penalize_unfinalized
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~previous_sequencer
  ~previous_bid_rate
  ~ledger
  =
  let (action_state, sequencer, bid_rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = previous_bid_rate
        ; finalized = false
        }
      ; valid_while =
        { lower = macroslot_lower macroslot
        ; upper = macroslot_lower macroslot + macroslot_switch_boundary
        }
      ; action_state
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -bid_rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              bid_rate * bid_deposit_factor
            }
          ]
        }
      ]
    }
  ]

(* The case where we switch the sequencer,
   but the previous sequencer has not finalized more than once.
*)
let do_switch_sequencer_penalize_double_finalized
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~previous_sequencer
  ~previous_bid_rate
  ~ledger
  ~signature
  ~other_ledger
  =
  assert verify_signature ~signature ~public_key:previous_sequencer ~data:(other_ledger, macroslot - 1)
  let (action_state, sequencer, bid_rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = previous_bid_rate
        ; finalized = true
        }
      ; valid_while =
        { lower = macroslot_lower macroslot
        ; upper = macroslot_lower macroslot + macroslot_switch_boundary
        }
      ; action_state
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -bid_rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              bid_rate * bid_deposit_factor
            }
          ]
        }
      ]
    }
  ]

(* Commit work to L1. *)
let do_commit
  ~txn_snark
  ~sequencer
  ~macroslot
  ~bid_rate
  ~valid_while
  ~new_actions
  =
  (* Currently it isn't checked whether the valid_while is within the macroslot.
     This can happen if switching isn't done. Consider whether this is sound. *)
  assert valid_while.upper - valid_while.lower <= max_valid_while_size ;

  let old_inner = get_account inner_pk txn_snark.source in
  let new_inner = get_account inner_pk txn_snark.target in

  let action_state =
    (* We don't force sequencer to match on latest action state,
       since it's unreliable and might roll back. *)
    List.append new_actions new_inner.app_state.outer_action_state in

  let ledger = txn_snark.target in
  let inner_action_state = new_inner.action_state in

  [ { public_key = zeko_pk
    ; actions = Commit { ledger ; inner_action_state ; valid_while }
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger = txn_snark.source
        ; inner_action_state = old_inner.action_state
        ; macroslot
        ; sequencer
        ; bid_rate
        ; finalized = false
        }
      ; valid_while
      ; action_state
      }
    ; children =
      { public_key = sequencer
      ; use_full_commitment = true
      }
    }
  ]

(* End our turn *)
let do_finalize_macroslot
  ~sequencer
  ~macroslot
  ~bid_rate
  ~ledger
  ~signature
  =
  assert verify_signature ~signature ~public_key:sequencer ~data:(ledger, macroslot)
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; events = signature
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = true
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
        ; macroslot
        ; sequencer
        ; bid_rate
        ; finalized = false
        }
      ; valid_while =
        { lower = macroslot_lower macroslot
        ; upper = infinity
        }
      }
    }
  ]

let do_witness_outer ~aux ~children ~valid_while =
  [ { public_key = zeko_pk
    ; actions = [ Witness { aux ; children ; valid_while } ]
    ; preconditions =
      { children
      }
    }
  ]

let do_witness_inner ~aux ~children =
  [ { public_key = inner_pk
    ; actions = [ Witness { aux ; children } ]
    ; preconditions =
      { children
      }
    }
  ]

```
