# Simplified core rollup spec without sequencer auction

Everything besides `do_inner_step` happens on the L1, i.e. host ledger.

```ocaml
type ledger = account list

type commit =
  { ledger : ledger
  ; inner_action_state : action_state
  ; synchronized_outer_action_state : action_state
  ; sequencer : sequencer
  ; valid_while : valid_while
  }

and type outer_action =
  | Commit of commit
  | Witness of { aux : 'a ; children : account_update_forest ; valid_while : valid_while }

and type outer_action_state = outer_action list

type inner_action =
  | Witness of { aux : 'a ; children : account_update_forest }

type inner_action_state = inner_action list

type outer_app_state =
  { ledger : ledger
  ; inner_action_state : inner_action_state
  ; sequencer : public_key
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
      { outer_action_state = Some source_action_state
      }
    }
  ]

val max_valid_while_size : nat

val zeko_token_owner : account_id

(* Commit work to L1. *)
let do_commit
  ~txn_snark
  ~valid_while
  ~sequencer
  ~new_actions
  =
  (* doesn't need to be at the same time as macroslot,
     can be early or late depending on other factors *)
  assert valid_while.upper - valid_while.lower < max_valid_while_size ;

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
      ; sequencer
      }
    ; preconditions =
      { app_state =
        { ledger = txn_snark.source
        ; inner_action_state = old_inner.action_state
        ; sequencer
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
