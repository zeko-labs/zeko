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
            | `Proved of
              ( Account_update.Stable.V1.t
              , Zkapp_command.Digest.Account_update.t
              , Zkapp_command.Digest.Forest.t )
              Zkapp_command.Call_forest.t
              Or_error.t
            | `Executed of Mina_transaction.Transaction_hash.t Or_error.t ] )
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
      add t key (`Proved result)
end

type precomputed_forest =
  ( Account_update.Stable.V1.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.t

type preverify_fn = precomputed_forest -> unit Deferred.Or_error.t

type t =
  { proofs_memory : Proofs_memory.t
  ; provers : Zeko_prover.Client.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  ; verification_keys : Zeko_prover.Prover.Verification_key_hashes.t
  ; preverify_l1 : preverify_fn
  ; preverify_l2 : preverify_fn
  ; bridge_txn_fee : Currency.Fee.t
  }

let create ~provers ~proof_cache_db ~preverify_l1 ~preverify_l2 ~bridge_txn_fee
    =
  let%map verification_keys =
    Zeko_prover.Client.verification_keys provers >>| Or_error.ok_exn
  in
  { proofs_memory = Proofs_memory.create ~lifetime:Float.(60. * 60.)
  ; provers
  ; proof_cache_db
  ; verification_keys
  ; preverify_l1
  ; preverify_l2
  ; bridge_txn_fee
  }

(** For [Finalize_cancelled_deposit] and [Finalize_withdrawal] the helper
    account update lives at [forest[0].calls[0].calls[0]] (nested inside the
    helper_token_owner). [precompute_commitments] returns it with
    [authorization = None_given], so the unchecked apply path's
    is_signed/signature_verifies assertion would trip during preverify. This
    splices [helper_account_signature] into the right account update so
    preverify sees the same authorization the executor will eventually
    submit. *)
let attach_nested_helper_signature ~signature_kind (forest : precomputed_forest)
    helper_signature : precomputed_forest =
  match forest with
  | [ ( { elt =
            { calls =
                ( { elt = { calls = helper_tree :: helper_rest; _ } as hto_elt
                  ; _
                  } as hto_tree )
                :: action_rest
            ; _
            } as top_elt
        ; _
        } as top_tree )
    ] ->
      let new_inner_calls =
        Zkapp_command.Call_forest.cons ~signature_kind
          ~calls:helper_tree.elt.calls
          { helper_tree.elt.account_update with
            authorization = Control.Poly.Signature helper_signature
          }
          helper_rest
      in
      let new_hto_tree =
        { hto_tree with elt = { hto_elt with calls = new_inner_calls } }
      in
      let new_top =
        { top_tree with
          elt = { top_elt with calls = new_hto_tree :: action_rest }
        }
      in
      [ new_top ] |> Utils.rehash_forest ~signature_kind
  | _ ->
      failwith "attach_nested_helper_signature: unexpected forest layout"

(** Verify a Schnorr signature against the partial transaction commitment. The
    signing public key is taken to be that of the supplied account update. Used
    to check the [transferrer] signature on submitDeposit/submitWithdrawal and
    the [helper_account_signature] on the finalize* actions. *)
let verify_signature ~signature_kind ~tx_commitment ~public_key signature =
  match Signature_lib.Public_key.decompress public_key with
  | None ->
      Or_error.error_string "verify_signature: invalid public key"
  | Some pk ->
      if
        Signature_lib.Schnorr.Chunked.verify ~signature_kind signature
          (Snark_params.Tick.Inner_curve.of_affine pk)
          (Random_oracle_input.Chunked.field tx_commitment)
      then Ok ()
      else Or_error.error_string "verify_signature: signature does not verify"

let wrap_with_transferrer ~signature_kind transferrer calls =
  Zkapp_command.Call_forest.cons ~signature_kind transferrer calls

(* The transferrer is the user-supplied account update wrapped at the top of
   submitDeposit/submitWithdrawal forests. Reject anything that isn't a
   plain default-token transfer of [-(amount + bridge_fee)] from the user's
   account, signed with a constant-nonce precondition and no children. *)
let validate_transferrer ~expected_amount
    (transferrer : Account_update.Stable.Latest.t) =
  let proof_cache_db = Proof_cache_tag.create_identity_db () in
  let transferrer =
    Account_update.write_all_proofs_to_disk ~proof_cache_db transferrer
  in
  let forest =
    Zkapp_command.Call_forest.cons (* signature_kind does not matter here *)
      ~signature_kind:Mina_signature_kind.t_DEPRECATED transferrer []
  in
  let expected_balance_change =
    Currency.Amount.Signed.(negate (of_unsigned expected_amount))
  in
  let spec : Utils.Forest_shape.field list list =
    [ [ Token_id Token_id.default
      ; Balance_change expected_balance_change
      ; Increment_nonce true
      ; Use_full_commitment false
      ; Authorization_kind Signature
      ; Preconditions_constant_nonce_only
      ; Calls []
      ]
    ]
  in
  if Utils.Forest_shape.matches forest spec then Ok ()
  else
    Or_error.error_string
      "validate_transferrer: account update does not match expected shape"

let run_and_check_exn (input : 'input) out_typ
    (main :
         'input V.t
      -> ('a, _) Compile_simple.main_return Snark_params.Tick.Checked.t ) =
  Snark_params.Tick.run_and_check_exn
    (let%bind.Checked () = exists Typ.unit ~compute:(fun _ -> ()) in
     let%map.Checked { out; _ } = main (V.return input) in
     As_prover.read out_typ out )

let fold ~(init_fn : 'init_var -> 'stmt_var Checked.t)
    ~(step_fn : 'elm_var -> 'stmt_var -> 'stmt_var Checked.t)
    ~(init_typ : ('init_var, 'init_val) Typ.typ)
    ~(stmt_typ : ('stmt_var, 'stmt_val) Typ.typ)
    ~(elm_typ : ('elm_var, 'elm_val) Typ.typ) (init : 'init_val)
    (elms : 'elm_val list) =
  Snark_params.Tick.run_and_check_exn
    (let%bind.Checked init = exists init_typ ~compute:(fun _ -> init) in
     let%bind.Checked stmt = init_fn init in
     let%bind.Checked elms =
       exists
         (Typ.list ~length:(List.length elms) elm_typ)
         ~compute:(fun _ -> elms)
     in
     let%map.Checked stmt =
       Zeko_util.foldl elms ~init:stmt ~f:(fun acc elm -> step_fn elm acc)
     in
     As_prover.read stmt_typ stmt )

let execute_request ?label t ~logger ~(executor : Executor.t) (key, d) =
  let%bind () = d in
  Proofs_memory.get t.proofs_memory key
  |> Option.value_exn |> snd
  |> function
  | `Pending ->
      return (Error (Error.of_string "Should not be reachable"))
  | `Proved (Error e) ->
      return (Error e)
  | `Executed _ ->
      return (Error (Error.of_string "Already executed"))
  | `Proved (Ok forest) ->
      let command : Zkapp_command.t =
        { fee_payer =
            Account_update.Fee_payer.make
              ~body:
                { public_key = Signer_service.Signer.public_key executor.signer
                ; fee = t.bridge_txn_fee
                ; valid_until = None
                ; nonce = Account.Nonce.zero
                }
              ~authorization:Signature.dummy
        ; account_updates =
            Zkapp_command.Call_forest.map forest
              ~f:
                (Account_update.write_all_proofs_to_disk
                   ~proof_cache_db:(Proof_cache_tag.create_identity_db ()) )
        ; memo = Signed_command_memo.empty
        }
      in
      let () =
        match label with
        | None ->
            ()
        | Some label ->
            printf "%s: %s\n%!" label
              (Yojson.Safe.to_string (Zkapp_command.to_yojson command))
      in
      let%map result = Executor.send_zkapp_command ~logger executor command in
      Proofs_memory.add t.proofs_memory key (`Executed result) ;
      result

module Deposit_request = struct
  type t =
    { deposit_params : Bridge_state.Deposit_params_base.t
    ; transferrer : Account_update.Stable.Latest.t
    }

  module Key = struct
    type t = { deposit_params : Bridge_state.Deposit_params_base.t }
    [@@deriving snarky]
  end

  let make_witness (deposit_params : Bridge_state.Deposit_params_base.t) :
      Bridge.Outer_action_witness.serializable =
    let receive_forest =
      Zkapp_command.Call_forest.cons
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
        ( Account_update.with_aux
            ~body:
              { Mina_base.Account_update.Body.dummy with
                use_full_commitment = true
              ; public_key = deposit_params.holder_account_l1
              ; balance_change =
                  Currency.Amount.Signed.(of_unsigned deposit_params.amount)
              ; may_use_token = Parents_own_token
              ; authorization_kind = None_given
              }
            ~authorization:Control.Poly.None_given
        |> Account_update.read_all_proofs_from_disk )
        []
    in
    let fee_payout_forest =
      Zkapp_command.Call_forest.cons
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
        ( Account_update.with_aux
            ~body:
              { Mina_base.Account_update.Body.dummy with
                use_full_commitment = true
              ; public_key = Zeko_circuits_config.Inputs.bridge_fee_recipient_l1
              ; balance_change =
                  Currency.Amount.Signed.of_unsigned
                    Zeko_circuits_config.Inputs.bridge_proof_fee
              ; may_use_token = Parents_own_token
              ; authorization_kind = None_given
              }
            ~authorization:Control.Poly.None_given
        |> Account_update.read_all_proofs_from_disk )
        []
    in
    { public_key = Zeko_circuits_config.Inputs.zeko_l1
    ; witness =
        { aux =
            Utils.value_to_hash ~init:Zeko_constants.deposit_salt
              Zeko_circuits.Bridge_state.Deposit_params_base.typ deposit_params
        ; children =
            Utils.rehash_forest
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
              (receive_forest @ fee_payout_forest)
        ; slot_range = Slot_range.infinite
        }
    }

  let precompute_commitments t ({ deposit_params; transferrer } : t) =
    let witness =
      Bridge.Outer_action_witness.of_serializable
        ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
        ~vk_hash:t.verification_keys.outer_rules
        (make_witness deposit_params)
    in
    try
      let _stmt, (body, _, calls) =
        run_and_check_exn witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Outer_rules_inst.Rule_action_witness_inst.main
      in
      let account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                body
            | None ->
                (* To make the fake tests work *)
                { body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 ~calls
          account_update []
        |> Zkapp_command.Call_forest.map
             ~f:Account_update.read_all_proofs_from_disk
        |> wrap_with_transferrer
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 transferrer
        |> Utils.rehash_forest
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
      in
      let tx_commitment =
        Zkapp_command.Transaction_commitment.create
          ~account_updates_hash:(Zkapp_command.Call_forest.hash forest)
      in
      Ok (forest, `Commitment tx_commitment)
    with exn -> Error (Error.of_exn exn)

  let key t (transferrer : Account_update.Stable.V1.t) =
    let transferrer_hash =
      Account_update.digest ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
        transferrer
    in
    let (Typ typ) = Key.typ in
    typ.value_to_fields t |> fst
    |> Array.append [| transferrer_hash |]
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.bridge_prover_cache)
    |> Field.to_string

  let f ~t ~logger ({ deposit_params; transferrer } : t) =
    let%bind.Result expected_amount =
      Currency.Amount.add deposit_params.amount
        Zeko_circuits_config.Inputs.bridge_proof_fee
      |> Result.of_option ~error:(Error.of_string "Amount overflow")
    in
    let%bind.Result () = validate_transferrer ~expected_amount transferrer in
    (* Verify the transferrer signature and preverify the command
       on L1 before spending compute on the proof. *)
    let%bind.Result forest, `Commitment commitment =
      precompute_commitments t { deposit_params; transferrer }
    in
    let%map.Result () =
      match Account_update.Poly.authorization transferrer with
      | Control.Poly.Signature signature ->
          verify_signature ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
            ~tx_commitment:commitment ~public_key:transferrer.body.public_key
            signature
      | _ ->
          Error (Error.of_string "Deposit_request: transferrer must be signed")
    in
    let key = key { deposit_params } transferrer in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let%bind () = t.preverify_l1 forest >>| Or_error.ok_exn in
                let witness = make_witness deposit_params in
                match%map
                  Zeko_prover.Client.outer_action_witness t.provers witness
                with
                | Error e ->
                    Error.raise e
                | Ok ((body, _, calls), proof) ->
                    Utils.attach_proof_to_forest
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                      ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                    |> wrap_with_transferrer
                         ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                         transferrer )
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
  type t =
    { withdrawal_params : Bridge_state.Withdrawal_params_base.t
    ; transferrer : Account_update.Stable.Latest.t
    }

  module Key = struct
    type t = { withdrawal_params : Bridge_state.Withdrawal_params_base.t }
    [@@deriving snarky]
  end

  let make_inner_receive_witness t
      (withdrawal_params : Bridge_state.Withdrawal_params_base.t) =
    Bridge.Inner_receive.of_serializable
      ~vk_hash:t.verification_keys.bridge_mina_l2
      { public_key = Zeko_circuits_config.Inputs.holder_account_l2
      ; amount = withdrawal_params.amount
      }

  let make_witness (withdrawal_params : Bridge_state.Withdrawal_params_base.t)
      inner_receive_forest : Bridge.Inner_action_witness.serializable =
    let fee_payout_forest =
      Zkapp_command.Call_forest.cons
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
        ( Account_update.with_aux
            ~body:
              { Mina_base.Account_update.Body.dummy with
                use_full_commitment = true
              ; public_key = Zeko_circuits_config.Inputs.bridge_fee_recipient_l2
              ; balance_change =
                  Currency.Amount.Signed.of_unsigned
                    Zeko_circuits_config.Inputs.bridge_proof_fee
              ; may_use_token = Parents_own_token
              ; authorization_kind = None_given
              }
            ~authorization:Control.Poly.None_given
        |> Account_update.read_all_proofs_from_disk )
        []
    in
    { public_key = Zeko_circuits_config.Inputs.inner_public_key
    ; witness =
        { aux =
            Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
              Zeko_circuits.Bridge_state.Withdrawal_params_base.typ
              withdrawal_params
        ; children =
            Utils.rehash_forest
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
              (inner_receive_forest @ fee_payout_forest)
        }
    }

  let precompute_commitments t ({ withdrawal_params; transferrer } : t) =
    let inner_receive_witness =
      make_inner_receive_witness t withdrawal_params
    in
    let _stmt, (inner_receive_body, _, inner_receive_calls) =
      run_and_check_exn inner_receive_witness
        Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
        Bridge_inst_mina.Rule_bridge_inner_receive.main
    in
    let inner_receive_forest =
      Zkapp_command.Call_forest.cons
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
        ~calls:inner_receive_calls
        (Account_update.with_aux
           ~body:
             ( match Is_compile_simple_real.is_compile_simple_real with
             | Some _ ->
                 inner_receive_body
             | None ->
                 { inner_receive_body with authorization_kind = None_given } )
           ~authorization:Control.Poly.None_given )
        []
      |> Zkapp_command.Call_forest.map
           ~f:Account_update.read_all_proofs_from_disk
    in
    let witness =
      Bridge.Inner_action_witness.of_serializable
        ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
        ~vk_hash:t.verification_keys.inner_rules
        (make_witness withdrawal_params inner_receive_forest)
    in
    try
      let _stmt, (body, _, calls) =
        run_and_check_exn witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Inner_rules_inst.Rule_inner_action_witness_inst.main
      in
      let account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                body
            | None ->
                (* To make the fake tests work *)
                { body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 ~calls
          account_update []
        |> Zkapp_command.Call_forest.map
             ~f:Account_update.read_all_proofs_from_disk
        |> wrap_with_transferrer
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 transferrer
        |> Utils.rehash_forest
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
      in
      let tx_commitment =
        Zkapp_command.Transaction_commitment.create
          ~account_updates_hash:(Zkapp_command.Call_forest.hash forest)
      in
      Ok (forest, `Commitment tx_commitment)
    with exn -> Error (Error.of_exn exn)

  let key t (transferrer : Account_update.Stable.V1.t) =
    let transferrer_hash =
      Account_update.digest ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
        transferrer
    in
    let (Typ typ) = Key.typ in
    typ.value_to_fields t |> fst
    |> Array.append [| transferrer_hash |]
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.bridge_prover_cache)
    |> Field.to_string

  let f ~t ~logger ({ withdrawal_params; transferrer } : t) =
    let bridge_fee = Zeko_circuits_config.Inputs.bridge_proof_fee in
    let%bind.Result expected_amount =
      Currency.Amount.add withdrawal_params.amount bridge_fee
      |> Result.of_option ~error:(Error.of_string "Amount overflow")
    in
    let%bind.Result () = validate_transferrer ~expected_amount transferrer in
    (* Verify the transferrer signature and preverify the command
       on L2 before spending compute on the proof. *)
    let%bind.Result forest, `Commitment commitment =
      precompute_commitments t { withdrawal_params; transferrer }
    in
    let%map.Result () =
      match Account_update.Poly.authorization transferrer with
      | Control.Poly.Signature signature ->
          verify_signature ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
            ~tx_commitment:commitment ~public_key:transferrer.body.public_key
            signature
      | _ ->
          Error
            (Error.of_string "Withdrawal_request: transferrer must be signed")
    in
    let key = key { withdrawal_params } transferrer in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let%bind () = t.preverify_l2 forest >>| Or_error.ok_exn in
                let%bind inner_receive_forest =
                  match%map
                    Zeko_prover.Client.inner_receive t.provers
                      { public_key =
                          Zeko_circuits_config.Inputs.holder_account_l2
                      ; amount = withdrawal_params.amount
                      }
                  with
                  | Error e ->
                      Error.raise e
                  | Ok ((body, _, calls), proof) ->
                      Utils.attach_proof_to_forest
                        ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                        ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                in
                let witness =
                  make_witness withdrawal_params inner_receive_forest
                in
                match%map
                  Zeko_prover.Client.inner_action_witness t.provers witness
                with
                | Error e ->
                    Error.raise e
                | Ok ((body, _, calls), proof) ->
                    Utils.attach_proof_to_forest
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                      ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                    |> wrap_with_transferrer
                         ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                         transferrer )
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
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
    }
  [@@deriving snarky]

  type t_ =
    { ase_source : Ase.With_length.Stmt.t
    ; check_accepted_init : Bridge_inst_mina.Check_accepted.Definition.Init.t
    ; prev_next_deposit : Zeko_util.Checked32.t
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
    ; ase_elems : Field.t list
    ; check_accepted_elems :
        Bridge_inst_mina.Check_accepted.Definition.Elem.t list
    }

  let precompute_commitments t
      ({ ase_source
       ; ase_elems
       ; check_accepted_init
       ; check_accepted_elems
       ; prev_next_deposit
       ; prev_nonce
       ; helper_account_new
       } :
        t_ ) =
    let deposit, check_accepted_elems =
      (List.hd_exn check_accepted_elems, List.tl_exn check_accepted_elems)
    in
    let deposit_hash =
      Zkapp_account.Actions_impl.hash [ Utils.actions_of_outer_action deposit ]
    in
    let ase =
      let target =
        fold
          ~init_fn:(Ase.M_with_length.init ~check:None)
          ~step_fn:Ase.M_with_length.step ~init_typ:Ase.M_with_length.Init.typ
          ~stmt_typ:Ase.M_with_length.Stmt.typ ~elm_typ:F.typ ase_source
          ase_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_deposit.Ase_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:ase_source ~proof_target:target ase_source []
    in
    let check_accepted =
      let source : Bridge.Check_accepted_mina.Stmt.t =
        { params = check_accepted_init.params
        ; action_state =
            Zkapp_account.Actions_impl.push_hash
              (Rollup_state.Outer_action_state.raw
                 check_accepted_init.original_action_state )
              deposit_hash
            |> Rollup_state.Outer_action_state.unsafe_value_of_field
        ; deposit_index = check_accepted_init.deposit_index
        ; n_steps = Zeko_util.Checked32.zero
        ; is_rejected = false
        ; is_accepted = false
        }
      in
      let target =
        fold
          ~init_fn:(Bridge_inst_mina.Check_accepted.Definition.init ~check:None)
          ~step_fn:Bridge_inst_mina.Check_accepted.Definition.step
          ~init_typ:Bridge_inst_mina.Check_accepted.Definition.Init.typ
          ~stmt_typ:Bridge_inst_mina.Check_accepted.Definition.Stmt.typ
          ~elm_typ:Bridge_inst_mina.Check_accepted.Definition.Elem.typ
          check_accepted_init check_accepted_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_deposit.Check_accepted_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:source ~proof_target:target check_accepted_init []
    in
    let witness : Bridge.Finalize_deposit.t =
      { vk_hash = t.verification_keys.bridge_mina_l2
      ; public_key = Zeko_circuits_config.Inputs.holder_account_l2
      ; may_use_token =
          Bridge_inst_mina.Rule_bridge_finalize_deposit.May_use_token.No
      ; inner_authorization_kind =
          Zeko_circuits.Rule_bridge_finalize_deposit.A.None_given
      ; ase
      ; check_accepted
      ; prev_next_deposit
      ; prev_nonce
      ; helper_account_new
      }
    in
    try
      let _stmt, (body, _, calls) =
        run_and_check_exn witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Bridge_inst_mina.Rule_bridge_finalize_deposit.main
      in
      let account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                body
            | None ->
                (* To make the fake tests work *)
                { body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 ~calls
          account_update []
        |> Zkapp_command.Call_forest.map
             ~f:Account_update.read_all_proofs_from_disk
        |> Utils.rehash_forest
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
      in
      let tx_commitment =
        Zkapp_command.Transaction_commitment.create
          ~account_updates_hash:(Zkapp_command.Call_forest.hash forest)
      in
      Ok (forest, `Commitment tx_commitment)
    with exn -> Error (Error.of_exn exn)

  let key
      ({ ase_source
       ; check_accepted_init
       ; prev_next_deposit
       ; prev_nonce
       ; helper_account_new
       ; ase_elems
       ; check_accepted_elems
       } :
        t_ ) =
    let (Typ typ) = typ in
    let (Typ check_accepted_elems_typ) =
      Bridge_inst_mina.Check_accepted.Definition.Elem.typ
    in
    let t =
      typ.value_to_fields
        { ase_source
        ; check_accepted_init
        ; prev_next_deposit
        ; prev_nonce
        ; helper_account_new
        }
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
         ~init:(Hash_prefix_create.salt Zeko_constants.bridge_prover_cache)
    |> Field.to_string

  let f ~t ~logger
      ({ ase_source
       ; ase_elems
       ; check_accepted_init
       ; check_accepted_elems
       ; prev_next_deposit
       ; prev_nonce
       ; helper_account_new
       } as request :
        t_ ) (helper_account_signature : Signature.t) =
    (* Verify the helper-account signature and preverify the
       command on L2 before spending compute on the proof. *)
    let%bind.Result forest, `Commitment commitment =
      precompute_commitments t request
    in
    let top_tree, helper, rest_calls =
      match forest with
      | [ ({ elt = { calls = helper :: rest; _ }; _ } as top) ] ->
          (top, helper, rest)
      | _ ->
          failwith "Finalize_deposit: unexpected precomputed forest layout"
    in
    let helper_pk = helper.elt.account_update.body.public_key in
    let%map.Result () =
      verify_signature ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
        ~tx_commitment:commitment ~public_key:helper_pk helper_account_signature
    in
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                (* Attach the helper signature in place before preverify so
                   [body.authorization_kind = Signature] matches the actual
                   authorization (otherwise the unchecked apply path's
                   is_signed/signature_verifies assertion fires). *)
                let forest =
                  let new_calls =
                    Zkapp_command.Call_forest.cons
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                      ~calls:helper.elt.calls
                      { helper.elt.account_update with
                        authorization =
                          Control.Poly.Signature helper_account_signature
                      }
                      rest_calls
                  in
                  [ { top_tree with
                      elt = { top_tree.elt with calls = new_calls }
                    }
                  ]
                  |> Utils.rehash_forest
                       ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                in
                let%bind () = t.preverify_l2 forest >>| Or_error.ok_exn in
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
                    ~prev_next_deposit ~prev_nonce ~helper_account_new
                with
                | Error e ->
                    Error.raise e
                | Ok ((body, _, calls), proof) ->
                    (* Attach helper account signature *)
                    let calls =
                      match calls with
                      | helper_account :: remaining_calls ->
                          Zkapp_command.Call_forest.cons
                            ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                            ~calls:helper_account.elt.calls
                            { helper_account.elt.account_update with
                              authorization =
                                Control.Poly.Signature helper_account_signature
                            }
                            remaining_calls
                      | _ ->
                          failwith "shouldn't be reachable"
                    in
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

module Finalize_cancelled_deposit = struct
  type t =
    { public_key : Public_key.Compressed.t
    ; commit : Rollup_state.Outer_action.Commit.t
    ; before_commit : Rollup_state.Outer_action_state.t
    ; commit_ase_source : Ase.Without_length.Stmt.t
    ; sync_ase_source : Ase.With_length.Stmt.t
    ; check_accepted_init : Bridge_inst_mina.Check_accepted.Definition.Init.t
    ; check_accepted_ase_source : Ase.With_length.Stmt.t
    ; prev_next_cancelled_deposit : Zeko_util.Checked32.t
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
    }
  [@@deriving snarky]

  type t_ =
    { public_key : Public_key.Compressed.t
    ; commit : Rollup_state.Outer_action.Commit.t
    ; before_commit : Rollup_state.Outer_action_state.t
    ; commit_ase_source : Ase.Without_length.Stmt.t
    ; sync_ase_source : Ase.With_length.Stmt.t
    ; check_accepted_init : Bridge_inst_mina.Check_accepted.Definition.Init.t
    ; check_accepted_ase_source : Ase.With_length.Stmt.t
    ; prev_next_cancelled_deposit : Zeko_util.Checked32.t
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
    ; commit_ase_elems : Field.t list
    ; sync_ase_elems : Field.t list
    ; check_accepted_elems :
        Bridge_inst_mina.Check_accepted.Definition.Elem.t list
    ; check_accepted_ase_elems : Field.t list
    }

  let precompute_commitments t
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; sync_ase_source
       ; check_accepted_init
       ; check_accepted_ase_source
       ; prev_next_cancelled_deposit
       ; commit_ase_elems
       ; sync_ase_elems
       ; check_accepted_elems
       ; check_accepted_ase_elems
       ; prev_nonce
       ; helper_account_new
       } :
        t_ ) =
    let deposit, check_accepted_elems =
      (List.hd_exn check_accepted_elems, List.tl_exn check_accepted_elems)
    in
    let deposit_hash =
      Zkapp_account.Actions_impl.hash [ Utils.actions_of_outer_action deposit ]
    in
    let commit_ase =
      let target =
        fold
          ~init_fn:(Ase.M_without_length.init ~check:None)
          ~step_fn:Ase.M_without_length.step
          ~init_typ:Ase.M_without_length.Init.typ
          ~stmt_typ:Ase.M_without_length.Stmt.typ ~elm_typ:F.typ
          commit_ase_source commit_ase_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.Ase_outer_inst
      .make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:commit_ase_source ~proof_target:target commit_ase_source
        []
    in
    let sync_ase =
      let target =
        fold
          ~init_fn:(Ase.M_with_length.init ~check:None)
          ~step_fn:Ase.M_with_length.step ~init_typ:Ase.M_with_length.Init.typ
          ~stmt_typ:Ase.M_with_length.Stmt.typ ~elm_typ:F.typ sync_ase_source
          sync_ase_elems
      in
      Bridge.Finalize_cancelled_deposit.Ase_outer_with_length_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:sync_ase_source ~proof_target:target sync_ase_source []
    in
    let check_accepted =
      let source : Bridge.Check_accepted_mina.Stmt.t =
        { params = check_accepted_init.params
        ; action_state =
            Zkapp_account.Actions_impl.push_hash
              (Rollup_state.Outer_action_state.raw
                 check_accepted_init.original_action_state )
              deposit_hash
            |> Rollup_state.Outer_action_state.unsafe_value_of_field
        ; deposit_index = check_accepted_init.deposit_index
        ; n_steps = Zeko_util.Checked32.zero
        ; is_rejected = false
        ; is_accepted = false
        }
      in
      let target =
        fold
          ~init_fn:(Bridge_inst_mina.Check_accepted.Definition.init ~check:None)
          ~step_fn:Bridge_inst_mina.Check_accepted.Definition.step
          ~init_typ:Bridge_inst_mina.Check_accepted.Definition.Init.typ
          ~stmt_typ:Bridge_inst_mina.Check_accepted.Definition.Stmt.typ
          ~elm_typ:Bridge_inst_mina.Check_accepted.Definition.Elem.typ
          check_accepted_init check_accepted_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
      .Check_accepted_inst
      .make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:source ~proof_target:target check_accepted_init []
    in
    let check_accepted_ase =
      let target =
        fold
          ~init_fn:(Ase.M_with_length.init ~check:None)
          ~step_fn:Ase.M_with_length.step ~init_typ:Ase.M_with_length.Init.typ
          ~stmt_typ:Ase.M_with_length.Stmt.typ ~elm_typ:F.typ
          check_accepted_ase_source check_accepted_ase_elems
      in
      Bridge.Finalize_cancelled_deposit.Ase_outer_with_length_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:check_accepted_ase_source ~proof_target:target
        check_accepted_ase_source []
    in
    let verify_two_outer_ases =
      run_and_check_exn (commit_ase, sync_ase)
        Typ.(
          Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.Ase_outer_inst
          .Stmt
          .typ
          * Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
            .Ase_outer_with_length_inst
            .Stmt
            .typ)
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_two_outer_ases
        .main
      |> Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_two_outer_ases
         .make_unchecked
           ~proof:
             (Compile_simple.Proof.of_pickles
                Pickles_types.Nat.(
                  Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
    in
    let verify_check_accepted_and_ase =
      run_and_check_exn
        (check_accepted, check_accepted_ase)
        Typ.(
          Bridge_inst_mina.Check_accepted.Definition.Stmt.typ
          * Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
            .Ase_outer_with_length_inst
            .Stmt
            .typ)
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_check_accepted_and_ase
        .main
      |> Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_check_accepted_and_ase
         .make_unchecked
           ~proof:
             (Compile_simple.Proof.of_pickles
                Pickles_types.Nat.(
                  Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
    in
    let witness : Bridge.Finalize_cancelled_deposit.t =
      { vk_hash = t.verification_keys.bridge_mina_l1
      ; helper_token_owner_l1_vk_hash =
          t.verification_keys.bridge_mina_token_owner
      ; public_key
      ; may_use_token =
          Bridge_inst_mina.Rule_bridge_finalize_withdrawal.May_use_token.No
      ; outer_authorization_kind =
          Zeko_circuits.Rule_bridge_finalize_withdrawal.A.None_given
      ; commit
      ; before_commit_ase = before_commit
      ; verify_two_outer_ases
      ; verify_check_accepted_and_ase
      ; prev_next_cancelled_deposit
      ; prev_nonce
      ; helper_account_new
      }
    in
    try
      let _stmt, (body, _, calls) =
        run_and_check_exn witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.main
      in
      let helper_account, witness_outer, remaining_calls =
        match calls with
        | { elt =
              { account_update = _helper_token_owner
              ; calls =
                  [ { elt = { account_update = helper_account; calls = []; _ }
                    ; _
                    }
                  ]
              ; _
              }
          ; _
          }
          :: { elt = { account_update = witness_outer; calls = []; _ }; _ }
             :: remaining_calls ->
            (helper_account, witness_outer, remaining_calls)
        | _ ->
            failwith
              "finalize_withdrawal precompute: invalid helper/witness layout"
      in
      let helper_witness =
        Bridge.Outer_token_owner.of_serializable
          ~vk_hash:t.verification_keys.bridge_mina_token_owner
          { public_key = Zeko_circuits_config.Inputs.helper_token_owner_l1
          ; a = helper_account.body
          }
      in
      let _helper_stmt, (helper_body, _, helper_calls) =
        run_and_check_exn helper_witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Bridge_inst_mina.Rule_bridge_outer_token_owner.main
      in
      let helper_account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                helper_body
            | None ->
                { helper_body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let helper_forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~calls:helper_calls helper_account_update []
      in
      let witness_forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 witness_outer []
      in
      let account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                body
            | None ->
                (* To make the fake tests work *)
                { body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~calls:(helper_forest @ witness_forest @ remaining_calls)
          account_update []
        |> Zkapp_command.Call_forest.map
             ~f:Account_update.read_all_proofs_from_disk
        |> Utils.rehash_forest
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
      in
      let tx_commitment =
        Zkapp_command.Transaction_commitment.create
          ~account_updates_hash:(Zkapp_command.Call_forest.hash forest)
      in
      Ok (forest, `Commitment tx_commitment)
    with exn -> Error (Error.of_exn exn)

  let key
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; sync_ase_source
       ; check_accepted_init
       ; check_accepted_ase_source
       ; prev_next_cancelled_deposit
       ; commit_ase_elems
       ; sync_ase_elems
       ; check_accepted_elems = _
       ; check_accepted_ase_elems
       ; prev_nonce
       ; helper_account_new
       } :
        t_ ) =
    let (Typ typ) = typ in
    let t =
      typ.value_to_fields
        { public_key
        ; commit
        ; before_commit
        ; commit_ase_source
        ; sync_ase_source
        ; check_accepted_init
        ; check_accepted_ase_source
        ; prev_next_cancelled_deposit
        ; prev_nonce
        ; helper_account_new
        }
      |> fst
    in
    let commit_ase_elems = Array.of_list commit_ase_elems in
    let sync_ase_elems = Array.of_list sync_ase_elems in
    let check_accepted_ase_elems = Array.of_list check_accepted_ase_elems in
    Array.append t commit_ase_elems
    |> Array.append sync_ase_elems
    |> Array.append check_accepted_ase_elems
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.bridge_prover_cache)
    |> Field.to_string

  let f ~t ~logger
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; sync_ase_source
       ; check_accepted_init
       ; check_accepted_ase_source
       ; prev_next_cancelled_deposit
       ; commit_ase_elems
       ; sync_ase_elems
       ; check_accepted_elems
       ; check_accepted_ase_elems
       ; prev_nonce
       ; helper_account_new
       } as request :
        t_ ) (helper_account_signature : Signature.t) =
    (* Verify the helper-account signature and preverify the
       command on L1 before spending compute on the proof. *)
    let%bind.Result forest, `Commitment commitment =
      precompute_commitments t request
    in
    let helper_pk =
      match forest with
      | [ { elt =
              { calls =
                  { elt =
                      { calls =
                          [ { elt = { account_update = helper_au; _ }; _ } ]
                      ; _
                      }
                  ; _
                  }
                  :: _
              ; _
              }
          ; _
          }
        ] ->
          helper_au.body.public_key
      | _ ->
          failwith
            "Finalize_cancelled_deposit: unexpected precomputed forest layout"
    in
    let%map.Result () =
      verify_signature ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
        ~tx_commitment:commitment ~public_key:helper_pk helper_account_signature
    in
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let forest =
                  attach_nested_helper_signature
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 forest
                    helper_account_signature
                in
                let%bind () = t.preverify_l1 forest >>| Or_error.ok_exn in
                let%bind ( (cancelled_deposit_body, _, calls)
                         , cancelled_deposit_proof ) =
                  let deposit, check_accepted_elems =
                    ( List.hd_exn check_accepted_elems
                    , List.tl_exn check_accepted_elems )
                  in
                  let deposit_hash =
                    Zkapp_account.Actions_impl.hash
                      [ Utils.actions_of_outer_action deposit ]
                  in
                  match%map
                    Zeko_prover.Client.finalize_cancelled_deposit t.provers
                      ~public_key
                      ~may_use_token:
                        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
                        .May_use_token
                        .No
                      ~outer_authorization_kind:
                        Zeko_circuits.Rule_bridge_finalize_cancelled_deposit.A
                        .None_given ~commit ~before_commit
                      ~commit_ase:(commit_ase_source, commit_ase_elems)
                      ~sync_ase:(sync_ase_source, sync_ase_elems)
                      ~check_accepted:
                        (check_accepted_init, deposit_hash, check_accepted_elems)
                      ~check_accepted_ase:
                        (check_accepted_ase_source, check_accepted_ase_elems)
                      ~prev_next_cancelled_deposit ~prev_nonce
                      ~helper_account_new
                  with
                  | Error e ->
                      Error.raise e
                  | Ok x ->
                      x
                in
                let helper_account, witness_outer, remaining_calls =
                  match calls with
                  | { elt =
                        { account_update = _helper_token_owner
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
                    :: { elt = { account_update = witness_outer; calls = []; _ }
                       ; _
                       }
                       :: remaining_calls ->
                      (helper_account, witness_outer, remaining_calls)
                  | _ ->
                      failwith
                        "cancel_deposit calls: no helper token owner or \
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
                      Error.raise e
                  | Ok ((body, _, calls), proof) ->
                      (* Attach helper account signature *)
                      let calls =
                        match calls with
                        | helper_account :: remaining_calls ->
                            Zkapp_command.Call_forest.cons
                              ~signature_kind:
                                Zeko_circuits_config.Inputs.chain_l1
                              ~calls:helper_account.elt.calls
                              { helper_account.elt.account_update with
                                authorization =
                                  Control.Poly.Signature
                                    helper_account_signature
                              }
                              remaining_calls
                        | _ ->
                            failwith "shouldn't be reachable"
                      in
                      Utils.attach_proof_to_forest
                        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                        ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                in
                let children =
                  helper_forest @ witness_forest @ remaining_calls
                in
                Utils.attach_proof_to_forest
                  ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                  ~proof_cache_db:t.proof_cache_db ~body:cancelled_deposit_body
                  ~calls:children ~proof:cancelled_deposit_proof
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
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
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
    ; prev_nonce : Zeko_util.Checked32.t
    ; helper_account_new : Zeko_util.Boolean.t
    ; commit_ase_elems : Field.t list
    ; withdrawal_ase_elems : Field.t list
    }

  let precompute_commitments t
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
       ; prev_nonce
       ; helper_account_new
       } :
        t_ ) =
    let commit_ase =
      let target =
        fold
          ~init_fn:(Ase.M_without_length.init ~check:None)
          ~step_fn:Ase.M_without_length.step
          ~init_typ:Ase.M_without_length.Init.typ
          ~stmt_typ:Ase.M_without_length.Stmt.typ ~elm_typ:F.typ
          commit_ase_source commit_ase_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_withdrawal.Ase_outer_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:commit_ase_source ~proof_target:target commit_ase_source
        []
    in
    let withdrawal_ase =
      let target =
        fold
          ~init_fn:(Ase.M_with_length.init ~check:None)
          ~step_fn:Ase.M_with_length.step ~init_typ:Ase.M_with_length.Init.typ
          ~stmt_typ:Ase.M_with_length.Stmt.typ ~elm_typ:F.typ
          withdrawal_ase_source withdrawal_ase_elems
      in
      Bridge_inst_mina.Rule_bridge_finalize_withdrawal.Ase_inner_inst.make
        ~proof:
          (Compile_simple.Proof.of_pickles
             Pickles_types.Nat.(Pickles.Proof.dummy N2.n N2.n ~domain_log2:14) )
        ~proof_source:withdrawal_ase_source ~proof_target:target
        withdrawal_ase_source []
    in
    let witness : Bridge.Finalize_withdrawal.t =
      { vk_hash = t.verification_keys.bridge_mina_l1
      ; public_key
      ; may_use_token =
          Bridge_inst_mina.Rule_bridge_finalize_withdrawal.May_use_token.No
      ; outer_authorization_kind =
          Zeko_circuits.Rule_bridge_finalize_withdrawal.A.None_given
      ; commit
      ; before_commit
      ; commit_ase
      ; before_withdrawal
      ; withdrawal_ase
      ; prev_next_withdrawal
      ; withdrawal_params
      ; helper_token_owner_l1_vk_hash =
          t.verification_keys.bridge_mina_token_owner
      ; l2_holder_vk_hash = t.verification_keys.bridge_mina_l2
      ; prev_nonce
      ; helper_account_new
      }
    in
    try
      let _stmt, (body, _, calls) =
        run_and_check_exn witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Bridge_inst_mina.Rule_bridge_finalize_withdrawal.main
      in
      let helper_account, witness_outer, remaining_calls =
        match calls with
        | { elt =
              { account_update = _helper_token_owner
              ; calls =
                  [ { elt = { account_update = helper_account; calls = []; _ }
                    ; _
                    }
                  ]
              ; _
              }
          ; _
          }
          :: { elt = { account_update = witness_outer; calls = []; _ }; _ }
             :: remaining_calls ->
            (helper_account, witness_outer, remaining_calls)
        | _ ->
            failwith
              "finalize_withdrawal precompute: invalid helper/witness layout"
      in
      let helper_witness =
        Bridge.Outer_token_owner.of_serializable
          ~vk_hash:t.verification_keys.bridge_mina_token_owner
          { public_key = Zeko_circuits_config.Inputs.helper_token_owner_l1
          ; a = helper_account.body
          }
      in
      let _helper_stmt, (helper_body, _, helper_calls) =
        run_and_check_exn helper_witness
          Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          Bridge_inst_mina.Rule_bridge_outer_token_owner.main
      in
      let helper_account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                helper_body
            | None ->
                { helper_body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let helper_forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~calls:helper_calls helper_account_update []
      in
      let witness_forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 witness_outer []
      in
      let account_update =
        Account_update.with_aux
          ~body:
            ( match Is_compile_simple_real.is_compile_simple_real with
            | Some _ ->
                body
            | None ->
                (* To make the fake tests work *)
                { body with authorization_kind = None_given } )
          ~authorization:Control.Poly.None_given
      in
      let forest =
        Zkapp_command.Call_forest.cons
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~calls:(helper_forest @ witness_forest @ remaining_calls)
          account_update []
        |> Zkapp_command.Call_forest.map
             ~f:Account_update.read_all_proofs_from_disk
        |> Utils.rehash_forest
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
      in
      let tx_commitment =
        Zkapp_command.Transaction_commitment.create
          ~account_updates_hash:(Zkapp_command.Call_forest.hash forest)
      in
      Ok (forest, `Commitment tx_commitment)
    with exn -> Error (Error.of_exn exn)

  let key
      ({ public_key
       ; commit
       ; before_commit
       ; commit_ase_source
       ; before_withdrawal
       ; withdrawal_ase_source
       ; prev_next_withdrawal
       ; withdrawal_params
       ; prev_nonce
       ; helper_account_new
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
        ; prev_nonce
        ; helper_account_new
        }
      |> fst
    in
    let commit_ase_elems = Array.of_list commit_ase_elems in
    let withdrawal_ase_elems = Array.of_list withdrawal_ase_elems in
    Array.append t commit_ase_elems
    |> Array.append withdrawal_ase_elems
    |> Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.bridge_prover_cache)
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
       ; prev_nonce
       ; helper_account_new
       } as request :
        t_ ) (helper_account_signature : Signature.t) =
    (* Verify the helper-account signature and preverify the
       command on L1 before spending compute on the proof. *)
    let%bind.Result forest, `Commitment commitment =
      precompute_commitments t request
    in
    let helper_pk =
      match forest with
      | [ { elt =
              { calls =
                  { elt =
                      { calls =
                          [ { elt = { account_update = helper_au; _ }; _ } ]
                      ; _
                      }
                  ; _
                  }
                  :: _
              ; _
              }
          ; _
          }
        ] ->
          helper_au.body.public_key
      | _ ->
          failwith "Finalize_withdrawal: unexpected precomputed forest layout"
    in
    let%map.Result () =
      verify_signature ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
        ~tx_commitment:commitment ~public_key:helper_pk helper_account_signature
    in
    let key = key request in
    ( key
    , Proofs_memory.prove t.proofs_memory key ~f:(fun () ->
          let%map result =
            try_with (fun () ->
                let forest =
                  attach_nested_helper_signature
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 forest
                    helper_account_signature
                in
                let%bind () = t.preverify_l1 forest >>| Or_error.ok_exn in
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
                      ~prev_next_withdrawal ~prev_nonce ~helper_account_new
                      ~withdrawal_params:
                        (Bridge.Finalize_withdrawal.Withdrawal_params_base
                         .to_serializable withdrawal_params )
                  with
                  | Error e ->
                      Error.raise e
                  | Ok x ->
                      x
                in
                let helper_account, witness_outer, remaining_calls =
                  match calls with
                  | { elt =
                        { account_update = _helper_token_owner
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
                    :: { elt = { account_update = witness_outer; calls = []; _ }
                       ; _
                       }
                       :: remaining_calls ->
                      (helper_account, witness_outer, remaining_calls)
                  | _ ->
                      failwith
                        "finalize_withdrawal calls: invalid helper/witness \
                         layout"
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
                      Error.raise e
                  | Ok ((body, _, calls), proof) ->
                      (* Attach helper account signature *)
                      let calls =
                        match calls with
                        | helper_account :: remaining_calls ->
                            Zkapp_command.Call_forest.cons
                              ~signature_kind:
                                Zeko_circuits_config.Inputs.chain_l1
                              ~calls:helper_account.elt.calls
                              { helper_account.elt.account_update with
                                authorization =
                                  Control.Poly.Signature
                                    helper_account_signature
                              }
                              remaining_calls
                        | _ ->
                            failwith "shouldn't be reachable"
                      in
                      Utils.attach_proof_to_forest
                        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                        ~proof_cache_db:t.proof_cache_db ~body ~calls ~proof
                in
                let children =
                  helper_forest @ witness_forest @ remaining_calls
                in
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
  |> function
  | `Pending | `Executed _ -> failwith "unreachable" | `Proved x -> x
