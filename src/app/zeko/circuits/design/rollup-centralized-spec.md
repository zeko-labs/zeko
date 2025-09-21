# Simplified core rollup spec without sequencer auction

Everything besides `do_inner_step` happens on the L1, i.e. host ledger.

```ocaml
val inner_pk : Public_key.t
val zeko_pk : Public_key.t

type ledger = account list

type commit =
  { ledger : ledger
  ; inner_action_state : action_state
  ; inner_action_state_length : nat
  ; synchronized_outer_action_state : action_state
  ; synchronized_outer_action_state_length : nat
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
  ; inner_action_state_length : nat
  ; sequencer : public_key
  ; pause_key : public_key
  ; da_key : public_key
  ; acc_set : indexed_merkle_tree
  ; is_paused : bool
  }

type inner_app_state =
  { outer_action_state : outer_action_state
  ; outer_action_state_length : nat
  }

(* happens inside rollup, on L2 *)
let do_inner_step ~source_action_state ~actions ~old_outer_action_state_length =
  [ { public_key = inner_pk
    ; app_state =
      { outer_action_state = List.append actions source_action_state
      ; outer_action_state_length = List.length actions + old_outer_action_state_length
      }
    ; preconditions =
      { outer_action_state = Some source_action_state
      ; outer_action_state_length = old_outer_action_state_length
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
  ~new_inner_actions
  ~old_inner_action_state_length
  ~unsynchronized_actions
  ~pause_key
  ~da_key
  =
  (* doesn't need to be at the same time as macroslot,
     can be early or late depending on other factors *)
  assert valid_while.upper - valid_while.lower < max_valid_while_size ;

  (* The vk for the inner account ensures that the outer action state as recorded only goes forward. *)
  let old_inner = get_account inner_pk txn_snark.source in
  let new_inner = get_account inner_pk txn_snark.target in

  let synchronized_outer_action_state = new_inner.app_state.outer_action_state in

  (* The new actions must be the difference between old synchronized outer action state and new synchronized outer action state. *)
  assert
    List.append new_actions old_inner.app_state.outer_action_state
    = synchronized_outer_action_state ;

  let synchronized_outer_action_state_length =
    List.length new_actions + old_inner.app_state.outer_action_state_length in

  let action_state =
    (* We don't force sequencer to match on latest action state,
       since it's unreliable and might roll back. *)
    List.append unsynchronized_actions synchronized_outer_action_state in

  let ledger = txn_snark.target in
  let inner_action_state = new_inner.action_state in
  let inner_action_state_length = List.length new_inner_actions + old_inner_action_state_length in

  [ { public_key = zeko_pk
    ; actions = Commit
      { ledger
      ; inner_action_state
      ; inner_action_state_length
      ; synchronized_outer_action_state
      ; synchronized_outer_action_state_length
      ; valid_while
      }
    ; app_state =
      { ledger
      ; inner_action_state
      ; inner_action_state_length
      ; sequencer
      ; paused = false
      ; pause_key
      ; da_key
      ; acc_set = txn_snark.target_acc_set
      }
    ; preconditions =
      { app_state =
        { ledger = txn_snark.source
        ; inner_action_state = old_inner.action_state
        ; inner_action_state_length = old_inner_action_state_length
        ; sequencer
        ; paused = false
        ; pause_key
        ; da_key
        ; acc_set = txn_snark.source_acc_set
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

let count_commits original_action_state actions =
  let f (acc_action_state, n) = function
    | (Commit _) as action ->
      List.append action acc_action_state, n + 1
    | (Witness _) as action ->
      List.append action acc_action_state, n
  in
  List.fold_left ~init:(original_action_state, 0) ~f actions

val max_sequencer_inactivity : nat

(* Emergency commit in case of sequencer's inactivity *)
let do_emergency_commit
  ~txn_snark
  ~valid_while
  ~new_actions
  ~new_inner_actions
  ~old_inner_action_state_length
  ~unsynchronized_actions
  ~pause_key
  ~da_key
  ~outer_action_state_before_last_commit
  ~last_commit
  ~actions_since_last_commit
  =
  let [ commit ] =
    do_commit
      ~txn_snark
      ~valid_while
      ~new_actions
      ~new_inner_actions
      ~old_inner_action_state_length
      ~unsynchronized_actions
      ~pause_key
      ~da_key
  in

  (* Count commits since last commit *)
  let (target_action_state, n_commits) = count_commits
    (List.append last_commit outer_action_state_before_last_commit)
    actions_since_last_commit
  in

  (* Check that there has not been any commit after last commit *)
  assert n_commits = 0 ;

  (* Check that count_commits is matching the precondition *)
  assert target_action_state = commit.preconditions.action_state ;

  (* Check that enought time elapsed since last commit *)
  assert commit.preconditions.valid_while.lower - last_commit.valid_while.upper >= max_sequencer_inactivity;

  (* Remove sequencer preconditions *)
  [ { commit with
      children = []
    ; preconditions.app_state.sequencer = Ignore
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

let init_inner =
  { account_id = inner_pk
  ; permissions = { all_proof with access = None }
  ; app_state =
    { outer_action_state = empty_action_state
    ; outer_action_state_length = 0
    }
  }

(* Not a rule, but instead a spec of how it's supposed to be initialized.
   In future, could have a circuit to verify that a zkapp account has been initialized
   correctly with the history as witness.

   Notably, there's nothing that says that the initial ledger must be empty.
   In fact, it _can't_ be empty, if it's to be useful.
   There should at least be the inner bridge account in addition
   to the rollup's own inner account, to facilitate bridges from the
   outside (L1) to the inside (L2), and vice-versa.

   Since the bridges are a separate contract entirely,
   they are not included in the spec here, though
   it is expected that there be only two accounts as mentioned above.
*)
let init_outer ~ledger ~sequencer ~pause_key ~da_key =
  assert ledger.(0) = init_inner in (* left-most account must be inner *)
  { account_id = zeko_pk
  ; permissions = { all_proof with access = None }
  ; app_state =
    { ledger
    ; inner_action_state = empty_action_state
    ; inner_action_state_length = 0
    ; sequencer
    ; pause_key
    ; da_key
    ; acc_set = Indexed_merkle_tree.from_list ([ 0 ; max ] @ ledger)
    ; is_paused = false
    }
  }
```
