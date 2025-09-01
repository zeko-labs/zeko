open Core_kernel
open Snark_params.Tick
open Async
open Mina_base
open Zeko_types
open Zeko_circuits
open Signature_lib

(**
  Hash table that holds the item only for the specified lifetime.
  Used to store proofs requested by users.
*)
module Proofs_memory = struct
  type t =
    { table :
        ( string
        , float
          * [ `Pending
            | `Done of
              ( Account_update.Stable.V1.t
              , Zkapp_command.Digest.Account_update.t
              , Zkapp_command.Digest.Forest.t )
              Zkapp_command.Call_forest.t
              Or_error.t ] )
        Hashtbl.t
    ; queue : (string * float) Queue.t
    ; lifetime : float
    }

  let create ~lifetime =
    { table = Hashtbl.create (module String)
    ; queue = Queue.create ()
    ; lifetime
    }

  let cleanup t =
    let now = Unix.gettimeofday () in
    let rec loop () =
      match Queue.peek t.queue with
      | Some (key, timestamp) when Float.(now -. timestamp > t.lifetime) ->
          Hashtbl.remove t.table key ;
          (Queue.dequeue_exn t.queue : string * float) |> ignore ;
          loop ()
      | _ ->
          ()
    in
    loop ()

  let add t key proof =
    let now = Unix.time () in
    Hashtbl.set t.table ~key ~data:(now, proof) ;
    Queue.enqueue t.queue (key, now) ;
    cleanup t

  let get t key = Hashtbl.find t.table key

  let mem t key = Hashtbl.mem t.table key

  let prove t key ~(f : unit -> 'a Deferred.t) =
    if mem t key then return ()
    else
      let () = add t key `Pending in
      let%map result = f () in
      add t key (`Done result)
end

type t =
  { proofs_memory : Proofs_memory.t
  ; provers : Zeko_prover.Client.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  }

let create ~provers ~proof_cache_db =
  { proofs_memory = Proofs_memory.create ~lifetime:Float.(60. * 20.)
  ; provers
  ; proof_cache_db
  }

module Deposit_request = struct
  type t = { deposit_params : Bridge_state.Deposit_params_base.t }
  [@@deriving snarky]

  let key t =
    let (Typ typ) = typ in
    typ.value_to_fields t |> fst
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
    |> Field.to_string

  let f ~t ~logger ({ deposit_params } as request : t) =
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let receive_forest =
                  Zkapp_command.Call_forest.cons
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                    (Account_update.with_no_aux
                       ~body:
                         { Mina_base.Account_update.Body.dummy with
                           use_full_commitment = true
                         ; public_key = deposit_params.holder_account_l1
                         ; balance_change =
                             Currency.Amount.Signed.(
                               of_unsigned deposit_params.amount)
                         ; may_use_token = Parents_own_token
                         ; authorization_kind = None_given
                         }
                       ~authorization:
                         (* Account_update.Checked.t == Account_update.Body.Checked.t so authorization is dropped anyways *)
                         Control.Poly.None_given )
                    []
                in
                match%map
                  Zeko_prover.Client.outer_action_witness t.provers
                    { public_key = Zeko_circuits_config.Inputs.zeko_l1
                    ; witness =
                        { aux =
                            Utils.value_to_hash
                              ~init:Zeko_constants.deposit_salt
                              Zeko_circuits.Bridge_state.Deposit_params_base.typ
                              deposit_params
                        ; children = receive_forest
                        ; slot_range = Slot_range.infinite
                        }
                    }
                with
                | Error e ->
                    failwith e
                | Ok ((body, _, calls), proof) ->
                    Utils.attach_proof_to_forest
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                      ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof )
            >>| Result.map_error ~f:(fun e -> Exn.to_string e)
          in
          match result with
          | Error e ->
              [%log warn] "prove failed %s" e ;
              Error (Error.of_string e)
          | Ok forest ->
              Ok forest ) )
end

module Withdrawal_request = struct
  type t = { withdrawal_params : Bridge_state.Withdrawal_params_base.t }
  [@@deriving snarky]

  let key t =
    let (Typ typ) = typ in
    typ.value_to_fields t |> fst
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
    |> Field.to_string

  let f ~t ~logger ({ withdrawal_params } as request : t) =
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let%bind inner_receive_forest =
                  match%map
                    Zeko_prover.Client.inner_receive t.provers
                      { public_key =
                          Zeko_circuits_config.Inputs.holder_account_l2
                      ; amount = withdrawal_params.amount
                      }
                  with
                  | Error e ->
                      failwith e
                  | Ok ((body, _, calls), proof) ->
                      Utils.attach_proof_to_forest
                        ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                        ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                in
                match%map
                  Zeko_prover.Client.inner_action_witness t.provers
                    { public_key = Zeko_circuits_config.Inputs.inner_public_key
                    ; witness =
                        { aux =
                            Utils.value_to_hash
                              ~init:Zeko_constants.withdrawal_salt
                              Zeko_circuits.Bridge_state.Withdrawal_params_base
                              .typ withdrawal_params
                        ; children = inner_receive_forest
                        }
                    }
                with
                | Error e ->
                    failwith e
                | Ok ((body, _, calls), proof) ->
                    Utils.attach_proof_to_forest
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                      ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof )
            >>| Result.map_error ~f:(fun e -> Exn.to_string e)
          in
          match result with
          | Error e ->
              [%log warn] "prove failed %s" e ;
              Error (Error.of_string e)
          | Ok forest ->
              Ok forest ) )
end

module Finalize_deposit = struct
  type t =
    { ase_source : Ase.With_length.Stmt.t
    ; check_accepted_init : Bridge_inst_mina.Check_accepted.Definition.Init.t
    ; prev_next_deposit : Zeko_util.Checked32.t
    }
  [@@deriving snarky]

  type t_ =
    { ase_source : Ase.With_length.Stmt.t
    ; check_accepted_init : Bridge_inst_mina.Check_accepted.Definition.Init.t
    ; prev_next_deposit : Zeko_util.Checked32.t
    ; ase_elems : Field.t list
    ; check_accepted_elems :
        Bridge_inst_mina.Check_accepted.Definition.Elem.t list
    }

  let key
      ({ ase_source
       ; check_accepted_init
       ; prev_next_deposit
       ; ase_elems
       ; check_accepted_elems
       } :
        t_ ) =
    let (Typ typ) = typ in
    let (Typ check_accepted_elems_typ) =
      Bridge_inst_mina.Check_accepted.Definition.Elem.typ
    in
    let t =
      typ.value_to_fields { ase_source; check_accepted_init; prev_next_deposit }
      |> fst
    in
    let ase_elems = Array.of_list ase_elems in
    let check_accepted_elems =
      List.map check_accepted_elems ~f:(fun x ->
          check_accepted_elems_typ.value_to_fields x |> fst |> Array.to_list )
      |> List.join |> List.to_array
    in
    Array.append t ase_elems
    |> Array.append check_accepted_elems
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
    |> Field.to_string

  let f ~t ~logger
      ({ ase_source
       ; ase_elems
       ; check_accepted_init
       ; check_accepted_elems
       ; prev_next_deposit
       } as request :
        t_ ) =
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let deposit, check_accepted_elems =
                  ( List.hd_exn check_accepted_elems
                  , List.tl_exn check_accepted_elems )
                in
                let deposit_hash =
                  Zkapp_account.Actions_impl.hash
                    [ Utils.actions_of_outer_action deposit ]
                in
                match%map
                  Zeko_prover.Client.finalize_deposit t.provers
                    ~public_key:Zeko_circuits_config.Inputs.holder_account_l2
                    ~may_use_token:
                      Bridge_inst_mina.Rule_bridge_finalize_deposit
                      .May_use_token
                      .No
                    ~inner_authorization_kind:
                      Zeko_circuits.Rule_bridge_finalize_deposit.A.None_given
                    ~ase:(ase_source, ase_elems)
                    ~check_accepted:
                      (check_accepted_init, deposit_hash, check_accepted_elems)
                    ~prev_next_deposit
                with
                | Error e ->
                    failwith e
                | Ok ((body, _, calls), proof) ->
                    Utils.attach_proof_to_forest
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                      ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                    |> Utils.rehash_forest
                         ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 )
            >>| Result.map_error ~f:(fun e -> Exn.to_string e)
          in
          match result with
          | Error e ->
              [%log warn] "prove failed %s" e ;
              Error (Error.of_string e)
          | Ok forest ->
              Ok forest ) )
end

module Finalize_withdrawal = struct
  type t =
    { public_key : Public_key.Compressed.t
    ; commit : Rollup_state.Outer_action.Commit.t
    ; before_commit : Rollup_state.Outer_action_state.t
    ; commit_ase_source : Ase.Without_length.Stmt.t
    ; before_withdrawal : Rollup_state.Inner_action_state.t
    ; withdrawal_ase_source : Ase.With_length.Stmt.t
    ; prev_next_withdrawal : Zeko_util.Checked32.t
    ; withdrawal_params : Bridge_state.Withdrawal_params_base.t
    }
  [@@deriving snarky]

  type t_ =
    { public_key : Public_key.Compressed.t
    ; commit : Rollup_state.Outer_action.Commit.t
    ; before_commit : Rollup_state.Outer_action_state.t
    ; commit_ase_source : Ase.Without_length.Stmt.t
    ; before_withdrawal : Rollup_state.Inner_action_state.t
    ; withdrawal_ase_source : Ase.With_length.Stmt.t
    ; prev_next_withdrawal : Zeko_util.Checked32.t
    ; withdrawal_params : Bridge_state.Withdrawal_params_base.t
    ; commit_ase_elems : Field.t list
    ; withdrawal_ase_elems : Field.t list
    }

  let key
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; before_withdrawal
       ; withdrawal_ase_source
       ; prev_next_withdrawal
       ; withdrawal_params
       ; commit_ase_elems
       ; withdrawal_ase_elems
       } :
        t_ ) =
    let (Typ typ) = typ in
    let t =
      typ.value_to_fields
        { public_key
        ; commit
        ; before_commit
        ; commit_ase_source
        ; before_withdrawal
        ; withdrawal_ase_source
        ; prev_next_withdrawal
        ; withdrawal_params
        }
      |> fst
    in
    let commit_ase_elems = Array.of_list commit_ase_elems in
    let withdrawal_ase_elems = Array.of_list withdrawal_ase_elems in
    Array.append t commit_ase_elems
    |> Array.append withdrawal_ase_elems
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
    |> Field.to_string

  let f ~t ~logger
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; commit_ase_elems
       ; before_withdrawal
       ; withdrawal_ase_source
       ; withdrawal_ase_elems
       ; prev_next_withdrawal
       ; withdrawal_params
       } as request :
        t_ ) =
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let%bind (withdrawal_body, _, calls), withdrawal_proof =
                  match%map
                    Zeko_prover.Client.finalize_withdrawal t.provers ~public_key
                      ~may_use_token:
                        Bridge_inst_mina.Rule_bridge_finalize_withdrawal
                        .May_use_token
                        .No
                      ~outer_authorization_kind:
                        Zeko_circuits.Rule_bridge_finalize_withdrawal.A
                        .None_given ~commit ~before_commit
                      ~commit_ase:(commit_ase_source, commit_ase_elems)
                      ~before_withdrawal
                      ~withdrawal_ase:
                        (withdrawal_ase_source, withdrawal_ase_elems)
                      ~prev_next_withdrawal
                      ~withdrawal_params:
                        (Bridge.Finalize_withdrawal.Withdrawal_params_base
                         .to_serializable withdrawal_params )
                  with
                  | Error e ->
                      failwith e
                  | Ok x ->
                      x
                in
                let (_, helper_account), witness_outer =
                  match calls with
                  | [ { elt =
                          { account_update = helper_token_owner
                          ; calls =
                              [ { elt =
                                    { account_update = helper_account
                                    ; calls = []
                                    ; _
                                    }
                                ; _
                                }
                              ]
                          ; _
                          }
                      ; _
                      }
                    ; { elt = { account_update = witness_outer; calls = []; _ }
                      ; _
                      }
                    ] ->
                      ((helper_token_owner, helper_account), witness_outer)
                  | _ ->
                      failwith
                        "finalize_withdrawal calls: no helper token owner or \
                         witness outer"
                in
                let witness_forest =
                  Zkapp_command.Call_forest.cons
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                    witness_outer []
                in
                let%map helper_forest =
                  match%map
                    Zeko_prover.Client.outer_token_owner t.provers
                      { public_key =
                          Zeko_circuits_config.Inputs.helper_token_owner_l1
                      ; a = helper_account.body
                      }
                  with
                  | Error e ->
                      failwith e
                  | Ok ((body, _, calls), proof) ->
                      Utils.attach_proof_to_forest
                        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                        ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                in
                let children = helper_forest @ witness_forest in
                Utils.attach_proof_to_forest
                  ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                  ~proof_cache_db:t.proof_cache_db ~body:withdrawal_body
                  ~calls:children ~proof:withdrawal_proof
                |> Utils.rehash_forest
                     ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 )
            >>| Result.map_error ~f:(fun e -> Exn.to_string e)
          in
          match result with
          | Error e ->
              [%log warn] "prove failed %s" e ;
              Error (Error.of_string e)
          | Ok forest ->
              Ok forest ) )
end

let prove t prover =
  let key, d = prover ~t in
  let%map () = d in
  Proofs_memory.get t.proofs_memory key
  |> Option.value_exn |> snd
  |> function `Pending -> failwith "unreachable" | `Done x -> x
