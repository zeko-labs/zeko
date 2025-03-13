open Core_kernel
open Mina_base
open Mina_ledger
open Mina_transaction_logic
open Zeko_prover.Zeko_types
module Field = Snark_params.Tick.Field

module Inputs = struct
  open struct
    module Zeko_local_state = Local_state
  end

  include Ledger.Inputs

  module Global_state = struct
    type t = { fee_excess : Amount.Signed.t; supply_increase : Amount.Signed.t }

    let fee_excess { fee_excess; _ } = fee_excess

    let set_fee_excess t fee_excess = { t with fee_excess }

    let supply_increase { supply_increase; _ } = supply_increase

    let set_supply_increase t supply_increase = { t with supply_increase }

    let block_global_slot _ = Global_slot_since_genesis.zero
  end

  module Call_stack = struct
    include Call_stack

    let with_hash t =
      let rec accumulate_call_stack_hashes
          ~(hash_frame : 'frame -> Mina_base.Stack_frame.Digest.t)
          (frames : 'frame list) :
          ('frame, Call_stack_digest.t) With_stack_hash.t list =
        match frames with
        | [] ->
            []
        | f :: fs ->
            let h_f = hash_frame f in
            let tl = accumulate_call_stack_hashes ~hash_frame fs in
            let h_tl =
              match tl with
              | [] ->
                  Call_stack_digest.empty
              | t :: _ ->
                  t.stack_hash
            in
            { stack_hash = Call_stack_digest.cons h_f h_tl; elt = f } :: tl
      in
      List.map t
        ~f:(With_hash.of_data ~hash_data:Mina_base.Stack_frame.Digest.create)
      |> accumulate_call_stack_hashes ~hash_frame:(fun x -> x.With_hash.hash)

    let hash with_hash =
      List.hd with_hash
      |> Option.value_map ~default:Call_stack_digest.empty
           ~f:With_stack_hash.stack_hash
  end

  module Local_state = struct
    include Local_state

    let to_zeko
        ({ stack_frame
         ; call_stack
         ; transaction_commitment
         ; full_transaction_commitment
         ; excess
         ; account_update_index
         } :
          ( Mina_base.Stack_frame.value
          , ( ( ( Token_id.t
                , ( Account_update.t
                  , Zkapp_command.Digest.Account_update.t
                  , Zkapp_command.Digest.Forest.t )
                  Zkapp_command.Call_forest.t )
                Mina_base.Stack_frame.t
              , Mina_base.Stack_frame.Digest.t )
              With_hash.t
            , Call_stack_digest.t )
            With_stack_hash.t
            list
          , Currency.Amount.Signed.t
          , Ledger.t
          , bool
          , Outer_action_state.t
          , Unsigned.uint32
          , 'a list )
          Zkapp_command_logic.Local_state.t ) : Zeko_local_state.t =
      Zeko_local_state.
        { stack_frame_digest = Mina_base.Stack_frame.Digest.create stack_frame
        ; call_stack_digest = Call_stack.hash call_stack
        ; transaction_commitment
        ; full_transaction_commitment
        ; excess
        ; account_update_index
        }
  end
end

type zeko_env =
  < zeko_mark_shifted_and_get_previous_shiftedness : Account_id.t -> bool >

let zeko_dummy_env : zeko_env =
  object
    method zeko_mark_shifted_and_get_previous_shiftedness _ = true
  end

let perform ~(zeko_env : zeko_env) ~global_slot (type r)
    (eff :
      ( r
      , < bool : Bool.t
        ; account : Account.t
        ; account_update : Account_update.t
        ; local_state : Inputs.Local_state.t
        ; .. > )
      Zkapp_command_logic.Eff.t ) : r =
  match eff with
  | Check_valid_while_precondition (valid_while, _global_state) ->
      Zkapp_precondition.Valid_while.check valid_while global_slot
      |> Or_error.is_ok
  | Check_protocol_state_precondition
      ((predicate : Zkapp_precondition.Protocol_state.t), _global_state) ->
      (* FIXME: Zkapp_precondition.Protocol_state.(equal predicate accept) *)
      (* Allow for global slot precondition, as it's the fee payer's valid while *)
      true
  | Check_account_precondition
      (account_update, account, new_account, local_state) ->
      let local_state = ref local_state in
      let check failure b =
        local_state := Inputs.Local_state.add_check !local_state failure b
      in
      Zkapp_precondition.Account.check ~new_account ~check
        account_update.body.preconditions.account account ;
      !local_state
  | Init_account { account_update = _; account = a } ->
      a
  | Get_shift_action_state a ->
      zeko_env#zeko_mark_shifted_and_get_previous_shiftedness
        (Account_id.create a.public_key a.token_id)

module Logic = Zkapp_command_logic.Make (Inputs)

let apply_signed_command_unchecked ~sequencer_pk ~constraint_constants
    ~global_slot ledger imt (command : Signed_command.t) =
  let source_ledger =
    let accounts_referenced =
      User_command.accounts_referenced (Signed_command command)
    in
    Sparse_ledger.of_ledger_subset_exn ledger accounts_referenced
  in
  let source_imt = Indexed_merkle_tree.Db.merkle_root imt in
  let%bind.Result status, new_accounts =
    Or_error.try_with_join (fun () ->
        match
          Ledger.apply_user_command_unchecked ~constraint_constants
            ~txn_global_slot:global_slot ledger command
        with
        | Ok
            { common = { user_command = { status; _ }; _ }
            ; body = Payment { new_accounts }
            } ->
            Ok (status, new_accounts)
        | Ok _ ->
            failwith "Internal error: it should be payment"
        | Error err ->
            Error err )
  in
  Ledger.commit ledger ;
  let update_acc_set_witness =
    let fee_payer = Signed_command.fee_payer command in
    let receiver = Signed_command.receiver command in
    let tids =
      List.map [ fee_payer; fee_payer; receiver ] ~f:(fun owner ->
          Account_id.derive_token_id ~owner )
    in
    List.fold tids ~init:Acc_set_witness.empty ~f:(fun acc tid ->
        let _, witness =
          Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
        in
        Acc_set_witness.add acc witness )
  in
  match status with
  | Applied ->
      Ok
        ( source_ledger
        , Base_input.
            { source_ledger = Sparse_ledger.merkle_root source_ledger
            ; source_acc_set = source_imt
            ; sequencer = sequencer_pk
            ; transaction = command
            ; witness =
                Base_witness.
                  { ledger_path_handler = source_ledger
                  ; update_acc_set_witness
                  }
            } )
  | Failed failures ->
      Or_error.error_string
        (sprintf "Transaction failed: %s"
           (Yojson.Safe.pretty_to_string
              (Transaction_status.Failure.Collection.to_yojson failures) ) )

let apply_zkapp_command_unchecked ~sequencer_pk ~zeko_env ~constraint_constants
    ~global_slot ledger imt archive (command : Zkapp_command.t) =
  let hash_local_state l =
    Zkapp_command_logic.Local_state.
      { l with call_stack = Inputs.Call_stack.with_hash l.call_stack }
  in
  let source_ledger =
    let accounts_referenced =
      User_command.accounts_referenced (Zkapp_command command)
    in
    Sparse_ledger.of_ledger_subset_exn ledger accounts_referenced
  in
  let state : Inputs.Global_state.t * _ Zkapp_command_logic.Local_state.t =
    let open Inputs in
    ( { fee_excess = Amount.(Signed.of_unsigned zero)
      ; supply_increase = Amount.(Signed.of_unsigned zero)
      }
    , { stack_frame =
          Stack_frame.make ~calls:(Call_forest.empty ())
            ~caller:Token_id.default ~caller_caller:Token_id.default
      ; call_stack = Call_stack.empty ()
      ; transaction_commitment = Transaction_commitment.empty
      ; full_transaction_commitment = Transaction_commitment.empty
      ; excess = Amount.(Signed.of_unsigned zero)
      ; supply_increase = Amount.(Signed.of_unsigned zero)
      ; ledger
      ; success = true
      ; account_update_index = Index.zero
      ; failure_status_tbl = []
      ; will_succeed = true
      } )
  in
  let account_updates = Zkapp_command.all_account_updates command in
  let perform eff = perform ~zeko_env:zeko_dummy_env ~global_slot eff in
  let witnesses =
    let l = hash_local_state (snd state) in
    let account_id = Zkapp_command.fee_payer command in
    [ ( account_id
      , fun ~imt_hash ~imt_witness ->
          Txn_snark_witness.Zkapp_command_segment.Single_unproved
            Zkapp_single_unproved_input.
              { base =
                  { source_ledger = Sparse_ledger.merkle_root source_ledger
                  ; source_local_state = Inputs.Local_state.to_zeko l
                  ; sequencer = sequencer_pk
                  ; source_acc_set = imt_hash
                  ; witness =
                      (let l = snd state in
                       Zkapp_rule_input_witness.
                         { stack_frame = l.stack_frame
                         ; call_stack = Inputs.Call_stack.with_hash l.call_stack
                         ; source_ledger_sparse = source_ledger
                         ; update_acc_set_witness = imt_witness
                         } )
                  }
              ; first =
                  (let account_updates =
                     Zkapp_command.all_account_updates command
                   in
                   Per_account_update.
                     { account_updates =
                         Zkapp_command.Call_forest.hash account_updates
                     ; account_updates_data = account_updates
                     ; memo_hash =
                         Signed_command_memo.hash @@ Zkapp_command.memo command
                     ; shift_action_state =
                         (* This should come from env, but would ruin later *)
                         true
                     } )
              } )
    ]
  in
  let%bind.Result state =
    Or_error.try_with (fun () ->
        Logic.start ~constraint_constants
          { account_updates
          ; memo_hash = Signed_command_memo.hash command.memo
          ; will_succeed = true
          }
          { perform } state )
  in
  let rec step_all (g, l) witnesses = function
    | [] ->
        Or_error.error_string "Internal error: empty account_updates"
    | account_update :: rest ->
        if
          List.is_empty
            Zkapp_command_logic.Local_state.(l.stack_frame.Stack_frame.calls)
        then Ok (l.failure_status_tbl, witnesses)
        else
          let account_id = Account_update.account_id account_update in
          let%bind.Result w =
            let incomplete_base ~imt_hash ~imt_witness =
              let l = hash_local_state (snd state) in
              Zkapp_rule_input.
                { source_ledger = Ledger.merkle_root ledger
                ; source_local_state = Inputs.Local_state.to_zeko l
                ; sequencer = sequencer_pk
                ; source_acc_set = imt_hash
                ; witness =
                    (let l = snd state in
                     Zkapp_rule_input_witness.
                       { stack_frame = l.stack_frame
                       ; call_stack = Inputs.Call_stack.with_hash l.call_stack
                       ; source_ledger_sparse =
                           Sparse_ledger.of_ledger_subset_exn ledger
                             [ account_id ]
                       ; update_acc_set_witness = imt_witness
                       } )
                }
            in
            let empty_start_data =
              Per_account_update.
                { account_updates_data =
                    Mina_base.Zkapp_command.Call_forest.accumulate_hashes' []
                ; memo_hash = Field.zero
                ; account_updates =
                    Mina_base.Zkapp_command.Call_forest.accumulate_hashes' []
                    |> Mina_base.Zkapp_command.Call_forest.hash
                ; shift_action_state = true
                }
            in
            match Account_update.authorization account_update with
            | None_given | Signature _ ->
                Ok
                  (fun ~imt_hash ~imt_witness ->
                    Txn_snark_witness.Zkapp_command_segment.Single_unproved
                      Zkapp_single_unproved_input.
                        { base = incomplete_base ~imt_hash ~imt_witness
                        ; first = empty_start_data
                        } )
            | Proof proof ->
                let%bind.Result vk =
                  (let%bind.Option loc =
                     Ledger.location_of_account ledger account_id
                   in
                   let%bind.Option acc = Ledger.get ledger loc in
                   let%bind.Option zkapp = Account.zkapp acc in
                   zkapp.verification_key )
                  |> Result.of_option
                       ~error:(Error.of_string "No verification key")
                in
                Ok
                  (fun ~imt_hash ~imt_witness ->
                    Txn_snark_witness.Zkapp_command_segment.Single_proved
                      Zkapp_single_proved_input.
                        { base = incomplete_base ~imt_hash ~imt_witness
                        ; first = empty_start_data
                        ; vk =
                            Compile_simple.Verification_key.of_pickles vk.data
                        ; zkapp_proof = Compile_simple.Proof.of_pickles proof
                        } )
          in
          let witnesses = (account_id, w) :: witnesses in
          let%bind.Result state =
            Or_error.try_with (fun () ->
                Logic.step ~constraint_constants { perform } (g, l) )
          in
          step_all state witnesses rest
  in
  let%bind.Result failures, witnesses =
    step_all state witnesses (Zkapp_command.Call_forest.to_list account_updates)
  in
  let%bind.Result () =
    if Transaction_status.Failure.Collection.is_empty failures then Ok ()
    else
      Or_error.error_string
        (sprintf "Transaction failed: %s"
           (Yojson.Safe.pretty_to_string
              (Transaction_status.Failure.Collection.to_yojson failures) ) )
  in
  Ledger.commit ledger ;
  let witnesses =
    List.map witnesses ~f:(fun (aid, incomplete_witness) ->
        let imt_hash = Indexed_merkle_tree.Db.merkle_root imt in
        let imt_witness =
          let _, w =
            Indexed_merkle_tree.Db.get_or_create_entry_exn imt
              (Account_id.derive_token_id ~owner:aid)
          in
          Acc_set_witness.(add empty w)
        in
        incomplete_witness ~imt_hash ~imt_witness )
  in

  (* let rec pair_unproved :
            Txn_snark_witness.Zkapp_command_segment.t list
         -> Txn_snark_witness.Zkapp_command_segment.t list = function
       | [] ->
           []
       | Single_unproved { base; first }
         :: Single_unproved
              { base = { witness = { update_acc_set_witness = second_imt } }
              ; first = second
              }
            :: rest ->
           let w =
             Zkapp_double_unproved_input.
               { base =
                   { base with
                     witness =
                       { base.witness with
                         update_acc_set_witness =
                           Acc_set_witness.join base.witness.update_acc_set_witness
                             second_imt
                       }
                   }
               ; first
               ; second
               }
           in
           Double_unproved w :: pair_unproved rest
       | hd :: tl ->
           hd :: pair_unproved tl
     in
     let witnesses = pair_unproved witnesses in *)

  (* Add events and actions to the memory *)
  let () =
    Zkapp_command.(
      Call_forest.iteri (account_updates command) ~f:(fun _ update ->
          let account =
            let account_id =
              Account_id.create
                (Account_update.public_key update)
                (Account_update.token_id update)
            in
            let location =
              Ledger.location_of_account ledger account_id
              |> Option.value_exn
                   ~message:"Internal error, account should be present"
            in
            Ledger.get ledger location
            |> Option.value_exn
                 ~message:"Internal error, account should be present"
          in
          Archive.add_account_update archive update account
            (Some
               Archive.Transaction_info.
                 { status = Applied
                 ; hash =
                     Mina_transaction.Transaction_hash.hash_command
                       (Zkapp_command command)
                 ; memo = Zkapp_command.memo command
                 ; authorization_kind =
                     Account_update.Body.authorization_kind update.body
                 } ) ))
  in
  Ok (source_ledger, witnesses)

let apply_user_command_unchecked ~sequencer_pk ~zeko_env ~constraint_constants
    ~global_slot ledger imt archive (command : User_command.t) =
  match command with
  | Signed_command ({ payload = { body = Payment _; _ }; _ } as command) ->
      let%map.Result source_ledger, w =
        apply_signed_command_unchecked ~sequencer_pk ~constraint_constants
          ~global_slot ledger imt command
      in
      (source_ledger, [ Txn_snark_witness.Signed_command w ])
  | Zkapp_command command ->
      let%map.Result source_ledger, w =
        apply_zkapp_command_unchecked ~sequencer_pk ~zeko_env
          ~constraint_constants ~global_slot ledger imt archive command
      in
      (source_ledger, List.map w ~f:(fun w -> Txn_snark_witness.Zkapp_command w))
  | Signed_command _ ->
      Or_error.error_string "Invalid signed command, we allow only payments"
