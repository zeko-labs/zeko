open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger
open Mina_transaction_logic
open Zeko_types
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
         ; _
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
          , Field.t
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
      ( ({ global_slot_since_genesis; _ } as protocol_state_predicate :
          Zkapp_precondition.Protocol_state.t )
      , _global_state ) ->
      Zkapp_precondition.Protocol_state.(
        equal protocol_state_predicate { accept with global_slot_since_genesis })
      && Zkapp_precondition.Numeric.(
           check ~label:"global_slot_since_genesis" Tc.global_slot)
           global_slot_since_genesis global_slot
         |> Or_error.is_ok
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
      |> List.map ~f:(fun id ->
             if
               Signature_lib.Public_key.Compressed.(
                 Account_id.public_key id = empty)
             then Zeko_constants.inner_account_id
             else id )
    in
    Sparse_ledger.of_ledger_subset_exn ledger accounts_referenced
  in
  let source_imt = Indexed_merkle_tree.Db.merkle_root imt in
  let%bind.Result status =
    Or_error.try_with_join (fun () ->
        match
          Ledger.apply_user_command_unchecked ~constraint_constants
            ~txn_global_slot:global_slot ledger command
        with
        | Ok { common = { user_command = { status; _ }; _ }; body = Payment _ }
          ->
            Ok status
        | Ok _ ->
            failwith "Internal error: it should be payment"
        | Error err ->
            Error err )
  in
  let%bind.Result () =
    match status with
    | Applied ->
        Ok ()
    | Failed failures ->
        Or_error.error_string
          (sprintf "Transaction failed: %s"
             (Yojson.Safe.pretty_to_string
                (Transaction_status.Failure.Collection.to_yojson failures) ) )
  in
  Ledger.commit ledger ;
  let update_acc_set_witness =
    let fee_payer = Signed_command.fee_payer command in
    let receiver = Signed_command.receiver command in
    let tids =
      List.map [ fee_payer; receiver; fee_payer ] ~f:(fun owner ->
          Account_id.derive_token_id ~owner )
    in
    List.fold tids ~init:Acc_set_witness.empty ~f:(fun acc tid ->
        let _, witness =
          Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
        in
        Acc_set_witness.add acc witness )
  in
  Ok
    ( source_ledger
    , Base_input.
        { source_ledger = Sparse_ledger.merkle_root source_ledger
        ; source_acc_set = Account_set.of_fields [| source_imt |]
        ; sequencer = sequencer_pk
        ; transaction = Command command
        ; witness =
            Base_witness.
              { ledger_path_handler = source_ledger; update_acc_set_witness }
        } )

let apply_zkapp_command_unchecked ~signature_kind ~sequencer_pk ~zeko_env
    ~constraint_constants ~global_slot ledger imt (command : Zkapp_command.t) =
  let hash_local_state l =
    Zkapp_command_logic.Local_state.
      { l with call_stack = Inputs.Call_stack.with_hash l.call_stack }
  in
  let read_stack_frame stack_frame =
    Stack_frame.
      { caller = stack_frame.caller
      ; caller_caller = stack_frame.caller_caller
      ; calls =
          Zkapp_command.Call_forest.map stack_frame.calls
            ~f:Account_update.read_all_proofs_from_disk
      }
  in
  let source_ledger =
    let accounts_referenced =
      User_command.accounts_referenced (Zkapp_command command)
      |> List.map ~f:(fun id ->
             if
               Signature_lib.Public_key.Compressed.(
                 Account_id.public_key id = empty)
             then Zeko_constants.inner_account_id
             else id )
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
  let all_account_updates =
    Zkapp_command.all_account_updates ~signature_kind command
  in
  let perform eff = perform ~zeko_env ~global_slot eff in
  let witnesses_rev =
    let l = hash_local_state (snd state) in
    let account_id =
      Account_update.account_id @@ Account_update.of_fee_payer
      @@ command.fee_payer
    in
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
                         { stack_frame = read_stack_frame l.stack_frame
                         ; call_stack =
                             Inputs.Call_stack.with_hash l.call_stack
                             |> List.map
                                  ~f:
                                    (With_stack_hash.map
                                       ~f:(With_hash.map ~f:read_stack_frame) )
                         ; source_ledger_sparse = source_ledger
                         ; update_acc_set_witness = imt_witness
                         } )
                  }
              ; first =
                  Per_account_update.
                    { account_updates =
                        Zkapp_command.Call_forest.hash all_account_updates
                    ; account_updates_data =
                        Zkapp_command.Call_forest.map
                          ~f:Account_update.read_all_proofs_from_disk
                          all_account_updates
                    ; memo_hash =
                        Signed_command_memo.hash
                        @@ Zkapp_command.Poly.memo command
                    ; shift_action_state =
                        (* This should come from env, but would ruin later *)
                        true
                    }
              } )
    ]
  in
  let%bind.Result state =
    Or_error.try_with (fun () ->
        Logic.start ~constraint_constants
          { account_updates = all_account_updates
          ; memo_hash = Signed_command_memo.hash command.memo
          ; will_succeed = true
          }
          { perform } state )
  in
  let rec step_all state witnesses_rev = function
    | [] ->
        let l = snd state in
        if
          List.is_empty
            Zkapp_command_logic.Local_state.(l.stack_frame.Stack_frame.calls)
        then Ok (l.failure_status_tbl, witnesses_rev)
        else Or_error.error_string "Internal error: empty account_updates"
    | account_update :: rest ->
        let account_id = Account_update.account_id account_update in
        let%bind.Result w =
          let source_ledger_sparse =
            Sparse_ledger.of_ledger_subset_exn ledger [ account_id ]
          in
          let l = hash_local_state (snd state) in
          let incomplete_base ~imt_hash ~imt_witness =
            Zkapp_rule_input.
              { source_ledger = Sparse_ledger.merkle_root source_ledger_sparse
              ; source_local_state = Inputs.Local_state.to_zeko l
              ; sequencer = sequencer_pk
              ; source_acc_set = imt_hash
              ; witness =
                  (let l = snd state in
                   Zkapp_rule_input_witness.
                     { stack_frame = read_stack_frame l.stack_frame
                     ; call_stack =
                         Inputs.Call_stack.with_hash l.call_stack
                         |> List.map
                              ~f:
                                (With_stack_hash.map
                                   ~f:(With_hash.map ~f:read_stack_frame) )
                     ; source_ledger_sparse
                     ; update_acc_set_witness = imt_witness
                     } )
              }
          in
          let empty_start_data =
            Per_account_update.
              { account_updates_data = []
              ; memo_hash = Field.zero
              ; account_updates = [] |> Mina_base.Zkapp_command.Call_forest.hash
              ; shift_action_state = true
              }
          in
          match Account_update.Poly.authorization account_update with
          | Control.Poly.None_given | Signature _ ->
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
                      ; vk = vk.data
                      ; zkapp_proof =
                          Compile_simple.Proof.of_pickles
                            (Proof_cache_tag.read_proof_from_disk proof)
                      } )
        in
        let witnesses_rev = (account_id, w) :: witnesses_rev in
        let%bind.Result state =
          Or_error.try_with (fun () ->
              Logic.step ~constraint_constants { perform } state )
        in
        step_all state witnesses_rev rest
  in
  let%bind.Result failures, witnesses_rev =
    step_all state witnesses_rev
      (Zkapp_command.Call_forest.to_list
         (Zkapp_command.Poly.account_updates command) )
  in
  let witnesses = List.rev witnesses_rev in
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
        let imt_hash =
          Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
        in
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
  Ok (source_ledger, witnesses)

let apply_user_command_unchecked ~signature_kind ~sequencer_pk ~zeko_env
    ~constraint_constants ~global_slot ledger imt (command : User_command.t) =
  match command with
  | Signed_command ({ payload = { body = Payment _; _ }; _ } as command) ->
      let%map.Result source_ledger, w =
        apply_signed_command_unchecked ~sequencer_pk ~constraint_constants
          ~global_slot ledger imt command
      in
      (source_ledger, [ Txn_snark_witness.Signed_command w ])
  | Zkapp_command command ->
      let%map.Result source_ledger, w =
        apply_zkapp_command_unchecked ~signature_kind ~sequencer_pk ~zeko_env
          ~constraint_constants ~global_slot ledger imt command
      in
      (source_ledger, List.map w ~f:(fun w -> Txn_snark_witness.Zkapp_command w))
  | Signed_command _ ->
      Or_error.error_string "Invalid signed command, we allow only payments"

let apply_fee_transfer_unchecked ~(receiver_pk : Even_PC.t) ~fee
    ~constraint_constants ~global_slot ledger imt =
  (* If the account is new, the current implementation of Indexed Merkle tree doesn't work with 2 new same accounts,
     therefore add inner account as second receiver with 0 fee. *)
  let command =
    Fee_transfer.of_singles
      (`Two
        ( Fee_transfer.Single.create
            ~receiver_pk:(Even_PC.to_pc receiver_pk)
            ~fee ~fee_token:Token_id.default
        , Fee_transfer.Single.create
            ~receiver_pk:Zeko_constants.inner_public_key ~fee:Currency.Fee.zero
            ~fee_token:Token_id.default ) )
    |> Or_error.ok_exn
  in
  let receiver_aid =
    Account_id.create (Even_PC.to_pc receiver_pk) Token_id.default
  in
  let source_ledger =
    let accounts_referenced =
      [ Zeko_constants.inner_account_id; receiver_aid ]
    in
    Sparse_ledger.of_ledger_subset_exn ledger accounts_referenced
  in
  let source_imt =
    Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
  in
  let%bind.Result status =
    Or_error.try_with_join (fun () ->
        match
          Ledger.apply_fee_transfer ~constraint_constants
            ~txn_global_slot:global_slot ledger command
        with
        | Ok { fee_transfer = { status; _ }; _ } ->
            Ok status
        | Error err ->
            Error err )
  in
  let%bind.Result () =
    match status with
    | Applied ->
        Ok ()
    | Failed failures ->
        Or_error.error_string
          (sprintf "Fee transfer failed: %s"
             (Yojson.Safe.pretty_to_string
                (Transaction_status.Failure.Collection.to_yojson failures) ) )
  in
  Ledger.commit ledger ;
  let update_acc_set_witness =
    let tids =
      List.map
        [ Zeko_constants.inner_account_id
        ; receiver_aid
        ; Zeko_constants.inner_account_id
        ] ~f:(fun owner -> Account_id.derive_token_id ~owner)
    in
    List.fold tids ~init:Acc_set_witness.empty ~f:(fun acc tid ->
        let _, witness =
          Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
        in
        Acc_set_witness.add acc witness )
  in
  Ok
    ( source_ledger
    , Txn_snark_witness.Signed_command
        Base_input.
          { source_ledger = Sparse_ledger.merkle_root source_ledger
          ; source_acc_set = source_imt
          ; sequencer = receiver_pk
          ; transaction = Fee_transfer command
          ; witness =
              Base_witness.
                { ledger_path_handler = source_ledger; update_acc_set_witness }
          } )

let status_to_or_error : Transaction_status.t -> unit Or_error.t = function
  | Applied ->
      Ok ()
  | Failed failures ->
      Or_error.error_string
        (sprintf "Transaction failed: %s"
           (Yojson.Safe.pretty_to_string
              (Transaction_status.Failure.Collection.to_yojson failures) ) )

(** Preverify a [User_command.t] by simulating its application against a sparse
    ledger built from accounts produced by [get_account]. Authorization
    (signatures and proofs) is NOT verified — only the state-transition logic
    (balances, nonces, preconditions, permissions, account creation, …) is
    checked. Useful for both L1 and L2: provide L1 [constraint_constants] and a
    [get_account] that fetches via GraphQL for L1, or L2 constants and a
    function that reads from the local ledger for L2. *)
let preverify_user_command
    ~(get_account : Account_id.t -> Account.t option Deferred.Or_error.t)
    ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    ~(global_slot : Mina_numbers.Global_slot_since_genesis.t)
    ~(state_view : Zkapp_precondition.Protocol_state.View.t)
    (command : User_command.t) : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let accounts_referenced = User_command.accounts_referenced command in
  let%bind accounts =
    Deferred.Or_error.List.map ~how:`Sequential accounts_referenced
      ~f:(fun aid ->
        let%map acc = get_account aid in
        (aid, acc) )
  in
  Deferred.return
  @@ Or_error.try_with_join (fun () ->
         Ledger.with_ephemeral_ledger ~depth:constraint_constants.ledger_depth
           ~f:(fun ledger ->
             List.iter accounts ~f:(fun (aid, acc) ->
                 match acc with
                 | None ->
                     ()
                 | Some acc ->
                     Ledger.create_new_account_exn ledger aid acc ) ;
             let sparse_ledger =
               Sparse_ledger.of_ledger_subset_exn ledger accounts_referenced
             in
             match command with
             | Signed_command sc ->
                 let (`If_this_is_used_it_should_have_a_comment_justifying_it
                       valid ) =
                   Signed_command.to_valid_unsafe sc
                 in
                 let open Or_error.Let_syntax in
                 let%bind _, applied =
                   Sparse_ledger.apply_user_command ~constraint_constants
                     ~txn_global_slot:global_slot sparse_ledger valid
                 in
                 status_to_or_error applied.common.user_command.status
             | Zkapp_command zc ->
                 let open Or_error.Let_syntax in
                 let%bind partial_txn, states =
                   Sparse_ledger
                   .apply_zkapp_first_pass_unchecked_with_states
                     ~constraint_constants ~global_slot ~state_view
                     ~fee_excess:Currency.Amount.Signed.zero
                     ~supply_increase:Currency.Amount.Signed.zero
                     ~first_pass_ledger:sparse_ledger
                     ~second_pass_ledger:sparse_ledger zc
                 in
                 let%bind applied, _ =
                   Sparse_ledger
                   .apply_zkapp_second_pass_unchecked_with_states ~init:states
                     sparse_ledger partial_txn
                 in
                 status_to_or_error applied.command.status ) )
