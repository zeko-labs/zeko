open Core_kernel
open Async
open Mina_base
open Zeko_types
module Field = Snark_params.Tick.Field

(**
  Hash table that holds the item only for the specified lifetime.
  Used to store proofs requested by users.
*)
module Proofs_memory = struct
  type t =
    { table :
        ( string
        , float
          * ( Account_update.Stable.V1.t
            , Zkapp_command.Digest.Account_update.t
            , Zkapp_command.Digest.Forest.t )
            Zkapp_command.Call_forest.t
            Or_error.t )
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
end

type t =
  { proofs_memory : Proofs_memory.t
  ; provers : Zeko_prover.Client.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  }

let create ~provers ~proof_cache_db =
  { proofs_memory = Proofs_memory.create ~lifetime:Float.(60. * 10.)
  ; provers
  ; proof_cache_db
  }

let deposit_request t ~logger ~key
    ~(deposit_params : Bridge.Finalize_deposit.Deposit_params_base.t) =
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
                     Currency.Amount.Signed.(of_unsigned deposit_params.amount)
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
                    Utils.value_to_hash ~init:Zeko_constants.deposit_salt
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
      Proofs_memory.add t.proofs_memory key (Error (Error.of_string e))
  | Ok forest ->
      Proofs_memory.add t.proofs_memory key (Ok forest)

let withdrawal_request t ~logger ~key
    ~(withdrawal_params : Bridge.Finalize_withdrawal.Withdrawal_params_base.t) =
  let%map result =
    try_with (fun () ->
        let%bind inner_receive_forest =
          match%map
            Zeko_prover.Client.inner_receive t.provers
              { public_key = Zeko_circuits_config.Inputs.holder_account_l2
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
                    Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
                      Zeko_circuits.Bridge_state.Withdrawal_params_base.typ
                      withdrawal_params
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
      Proofs_memory.add t.proofs_memory key (Error (Error.of_string e))
  | Ok forest ->
      Proofs_memory.add t.proofs_memory key (Ok forest)

let finalize_deposit t ~logger ~key
    ~(ase : Ase.With_length.Stmt.t * Field.t list)
    ~(check_accepted :
       Bridge.Finalize_deposit.Check_accepted_mina.Init.t
       * Bridge.Finalize_deposit.Check_accepted_mina.Elem.t list )
    ~prev_next_deposit =
  let%map result =
    try_with (fun () ->
        let check_accepted_init, check_accepted_elems = check_accepted in
        let deposit, check_accepted_elems =
          (List.hd_exn check_accepted_elems, List.tl_exn check_accepted_elems)
        in
        let deposit_hash =
          Zkapp_account.Actions_impl.hash
            [ Utils.actions_of_outer_action deposit ]
        in
        match%map
          Zeko_prover.Client.finalize_deposit t.provers
            ~public_key:Zeko_circuits_config.Inputs.holder_account_l2
            ~may_use_token:
              Bridge_inst_mina.Rule_bridge_finalize_deposit.May_use_token.No
            ~inner_authorization_kind:
              Zeko_circuits.Rule_bridge_finalize_deposit.A.None_given ~ase
            ~check_accepted:
              (check_accepted_init, deposit_hash, check_accepted_elems)
            ~prev_next_deposit
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
      Proofs_memory.add t.proofs_memory key (Error (Error.of_string e))
  | Ok forest ->
      Proofs_memory.add t.proofs_memory key (Ok forest)

let finalize_withdrawal t ~logger ~key ~public_key ~commit ~before_commit
    ~commit_ase ~before_withdrawal ~withdrawal_ase ~prev_next_withdrawal
    ~withdrawal_params =
  let%map result =
    try_with (fun () ->
        let%bind (withdrawal_body, _, calls), withdrawal_proof =
          match%map
            Zeko_prover.Client.finalize_withdrawal t.provers ~public_key
              ~may_use_token:
                Bridge_inst_mina.Rule_bridge_finalize_withdrawal.May_use_token
                .No
              ~outer_authorization_kind:
                Zeko_circuits.Rule_bridge_finalize_withdrawal.A.None_given
              ~commit ~before_commit ~commit_ase ~before_withdrawal
              ~withdrawal_ase ~prev_next_withdrawal ~withdrawal_params
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
                            { account_update = helper_account; calls = []; _ }
                        ; _
                        }
                      ]
                  ; _
                  }
              ; _
              }
            ; { elt = { account_update = witness_outer; calls = []; _ }; _ }
            ] ->
              ((helper_token_owner, helper_account), witness_outer)
          | _ ->
              failwith
                "finalize_withdrawal calls: no helper token owner or witness \
                 outer"
        in
        let witness_forest =
          Zkapp_command.Call_forest.cons
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 witness_outer
            []
        in
        let%map helper_forest =
          match%map
            Zeko_prover.Client.outer_token_owner t.provers
              { public_key = Zeko_circuits_config.Inputs.helper_token_owner_l1
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
          ~proof_cache_db:t.proof_cache_db ~body:withdrawal_body ~calls:children
          ~proof:withdrawal_proof )
    >>| Result.map_error ~f:(fun e -> Exn.to_string e)
  in
  match result with
  | Error e ->
      [%log warn] "prove failed %s" e ;
      Proofs_memory.add t.proofs_memory key (Error (Error.of_string e))
  | Ok forest ->
      Proofs_memory.add t.proofs_memory key (Ok forest)

let prove t prover =
  let key = Int.to_string @@ Random.int Int.max_value in
  let%map () = prover t ~key in
  Proofs_memory.get t.proofs_memory key |> Option.value_exn |> snd
