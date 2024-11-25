open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Checked.Let_syntax

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module Zeko_stmt = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; sequencer : Signature_lib.Public_key.Compressed.t
    ; fee_excess : Currency.Fee.Signed.t
    ; slot_range : Slot_range.t
    }
  [@@deriving snarky]
end

module T = struct
  type t = { stmt : Zeko_stmt.t; proof : Proof_V.t } [@@deriving snarky]
end

module Handler_V = Mk_V (Handler)

module Base_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : Handler_V.t
    ; sequencer : PC.t
    }
  [@@deriving snarky]
end

module Merge_input = struct
  type t = { left : T.t; right : T.t } [@@deriving snarky]
end

module Witness_V = Mk_V (Transaction_snark.Zkapp_command_segment.Witness)

module Zkapp_single_unproved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : Handler_V.t
    ; witness : Witness_V.t
    ; sequencer : PC.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

module Zkapp_double_unproved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : Handler_V.t
    ; witness : Witness_V.t
    ; sequencer : PC.t
    ; shift_action_state_first : Boolean.t
    ; shift_action_state_second : Boolean.t
    }
  [@@deriving snarky]
end

module Verification_key = struct
  include Pickles.Side_loaded.Verification_key

  type var = Checked.t
end

module Zkapp_single_proved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : Handler_V.t
    ; witness : Witness_V.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; sequencer : PC.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

let zeko_stmt_of_mina_stmt =
  let open struct
    let dummy_pc_init = Pending_coinbase.Stack.empty

    let genesis_constants = Genesis_constants.compiled

    let consensus_constants =
      Consensus.Constants.create ~constraint_constants
        ~protocol_constants:genesis_constants.protocol

    (** Dummy state body, network preconditions are disabled anyway *)
    let dummy_state_body =
      let compile_time_genesis =
        Mina_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
          ~constraint_constants ~consensus_constants
          ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
      in
      Mina_state.Protocol_state.body compile_time_genesis.data

    let dummy_pc =
      Pending_coinbase.Stack.push_state
        (Mina_state.Protocol_state.Body.hash dummy_state_body)
        Mina_numbers.Global_slot_since_genesis.zero dummy_pc_init
  end in
  let open Checked in
  fun ~(sequencer : PC.var) ~slot_range
      (stmt : Transaction_snark.Statement.With_sok.var) ->
    (* Check that pending_coinbase_stack is correctly set. This also constrains
       protocol state. See check_protocol_state in transaction_snark.ml. *)
    let dummy_pc = constant Pending_coinbase.Stack.typ dummy_pc in
    let* () =
      with_label __LOC__ (fun () ->
          Pending_coinbase.Stack.equal_var dummy_pc
            stmt.source.pending_coinbase_stack
          >>= Boolean.Assert.is_true )
    in
    let* () =
      with_label __LOC__ (fun () ->
          Pending_coinbase.Stack.equal_var dummy_pc
            stmt.target.pending_coinbase_stack
          >>= Boolean.Assert.is_true )
    in
    (* Check that transactions have been completely applied *)
    let empty_state = Mina_state.Local_state.(constant typ @@ empty ()) in
    let* () =
      with_label __LOC__ (fun () ->
          Mina_state.Local_state.Checked.assert_equal empty_state
            stmt.source.local_state
          |> Checked.return )
    in
    let* () =
      with_label __LOC__ (fun () ->
          Mina_state.Local_state.Checked.assert_equal empty_state
            stmt.target.local_state
          |> Checked.return )
    in

    (* Check that first and second passes are connected *)
    let* () =
      with_label __LOC__ (fun () ->
          Ledger_hash.assert_equal stmt.target.first_pass_ledger
            stmt.source.second_pass_ledger )
    in

    (* Check that it's a complete transaction (a "block") *)
    let* () =
      with_label __LOC__ (fun () ->
          Ledger_hash.assert_equal stmt.target.first_pass_ledger
            stmt.connecting_ledger_right )
    in
    let* () =
      with_label __LOC__ (fun () ->
          Ledger_hash.assert_equal stmt.source.second_pass_ledger
            stmt.connecting_ledger_left )
    in

    (*  No MINA must be minted *)
    let* is_neg =
      Currency.Amount.Signed.Checked.sgn stmt.supply_increase
      >>| Sgn.Checked.is_neg
    in
    let* is_zero =
      Currency.Amount.Signed.Checked.magnitude stmt.supply_increase
      >>= Currency.Amount.(Checked.equal (constant typ zero))
    in
    let* () =
      with_label __LOC__ (fun () ->
          Boolean.(is_neg || is_zero) >>= Boolean.Assert.is_true )
    in
    let*| () =
      Currency.Fee.(
        Signed.Checked.magnitude stmt.fee_excess.fee_excess_r
        >>= assert_equal ~label:__LOC__ typ (constant typ zero))
    in
    ( { source_ledger = stmt.source.first_pass_ledger
      ; target_ledger = stmt.target.second_pass_ledger
      ; sequencer
      ; fee_excess = stmt.fee_excess.fee_excess_l
      ; slot_range
      }
      : Zeko_stmt.var )

let account_with_hash (account : Account.Checked.Unhashed.t) :
    (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t =
  With_hash.of_data account ~hash_data:(fun a ->
      lazy
        (let a =
           { a with
             zkapp = (Zkapp_account.Checked.digest a.zkapp, ref (Some None))
           }
         in
         Run.run_checked (Account.Checked.digest a) ) )

let perform ~(shift_action_states : Boolean.var list)
    ~(set_slot_range : Slot_range.var -> unit) =
  let shift_action_states = ref shift_action_states in
  fun (type r)
      (eff :
        ( r
        , < bool : Boolean.var
          ; account :
              (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t
          ; local_state :
              ( Transaction_snark.Base.Zkapp_command_snark.stack_frame
              , Transaction_snark.Base.Zkapp_command_snark.call_stack
              , Currency.Amount.Signed.var
              , Ledger_hash.var * Mina_ledger.Sparse_ledger.t Prover_value.t
              , Boolean.var
              , Field.Var.t
              , Transaction_snark.Base.Zkapp_command_snark.length
              , unit )
              Mina_transaction_logic.Zkapp_command_logic.Local_state.t
          ; .. > )
        Mina_transaction_logic.Zkapp_command_logic.Eff.t ) : r ->
    match eff with
    | Check_valid_while_precondition
        ((valid_while : Zkapp_precondition.Valid_while.Checked.t), _global_state)
      ->
        let ({ lower; upper } : _ Zkapp_precondition.Closed_interval.t) =
          Zkapp_basic.Or_ignore.Checked.data valid_while
        in
        (* NB: We don't need to check whether valid_while is Some, because even if it is
           None, nothing will break.
        *)
        set_slot_range { lower; upper } ;
        (* We always return true because failure doesn't happen here but in the commit rule. *)
        Boolean.true_
    | Check_protocol_state_precondition
        ( (protocol_state_predicate :
            Zkapp_precondition.Protocol_state.Checked.t )
        , _global_state ) ->
        Run.run_checked
          Zkapp_precondition.Protocol_state.(
            assert_equal ~label:__LOC__ typ protocol_state_predicate
              (constant typ accept)) ;
        Boolean.true_
    | Check_account_precondition
        ( ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
        , (account : _ With_hash.t)
        , new_account
        , local_state ) ->
        let local_state = ref local_state in
        let check _failure b = Run.Boolean.Assert.is_true b in
        Zkapp_precondition.Account.Checked.check ~new_account ~check
          account_update.data.preconditions.account account.data ;
        !local_state
    | Init_account
        { account_update =
            ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
        ; account : (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t
        } ->
        (* FIXME: Add account duplication check. *)
        let account' : Account.Checked.Unhashed.t =
          { account.data with
            public_key = account_update.data.public_key
          ; token_id = account_update.data.token_id
          }
        in
        account_with_hash account'
    | Get_shift_action_state _ -> (
        match !shift_action_states with
        | [] ->
            failwith "unexpected"
        | x :: xs ->
            shift_action_states := xs ;
            x )

include
  ( val Compile_simple.compile ~override_wrap_domain:`N1
          ~name:"zeko-transaction-snark" ~out_typ:Zeko_stmt.typ
          ~branches:
            [ { branch_name = "base (user commands)"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { stmt; handler; sequencer } =
                      exists Base_input.typ ~compute:(V.get input)
                    in
                    let* () =
                      handle_as_prover
                        (fun () ->
                          Transaction_snark.Base.main ~constraint_constants stmt
                          )
                        (V.get handler)
                    in
                    let*| stmt =
                      zeko_stmt_of_mina_stmt ~sequencer
                        ~slot_range:Slot_range.(constant typ infinite)
                        stmt
                    in
                    Compile_simple.{ prevs = No_prevs; out = stmt } )
              }
            ; { branch_name = "single-unproved-zkapp"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { stmt
                         ; handler
                         ; witness
                         ; sequencer
                         ; shift_action_state
                         } =
                      exists Zkapp_single_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let slot_range = ref Slot_range.(constant typ infinite) in
                    let set_slot_range s = slot_range := s in
                    let* must_be_none, _must_verify_zkapp =
                      handle_as_prover
                        (fun () ->
                          let@ () = make_checked in
                          Transaction_snark.Base.Zkapp_command_snark.main
                            ~witness:
                              (V.unsafe_unwrap witness |> Option.value_exn)
                            ~zeko_handler:
                              { perform =
                                  (fun eff ->
                                    perform
                                      ~shift_action_states:
                                        [ shift_action_state ] ~set_slot_range
                                      eff )
                              }
                            ~constraint_constants
                            (Transaction_snark.Zkapp_command_segment.Basic
                             .to_single_list Opt_signed )
                            stmt )
                        (V.get handler)
                    in
                    assert (Option.is_none must_be_none) ;
                    let*| stmt =
                      zeko_stmt_of_mina_stmt ~sequencer ~slot_range:!slot_range
                        stmt
                    in
                    Compile_simple.{ prevs = No_prevs; out = stmt } )
              }
            ; { branch_name = "double-unproved-zkapp"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { stmt
                         ; handler
                         ; witness
                         ; sequencer
                         ; shift_action_state_first
                         ; shift_action_state_second
                         } =
                      exists Zkapp_double_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let slot_range = ref Slot_range.(constant typ infinite) in
                    let set_slot_range s = slot_range := s in
                    let* must_be_none, _must_verify_zkapp =
                      handle_as_prover
                        (fun () ->
                          let@ () = make_checked in
                          Transaction_snark.Base.Zkapp_command_snark.main
                            ?witness:(V.unsafe_unwrap witness)
                            ~zeko_handler:
                              { perform =
                                  (fun eff ->
                                    perform
                                      ~shift_action_states:
                                        [ shift_action_state_first
                                        ; shift_action_state_second
                                        ]
                                      ~set_slot_range eff )
                              }
                            ~constraint_constants
                            (Transaction_snark.Zkapp_command_segment.Basic
                             .to_single_list Opt_signed_opt_signed )
                            stmt )
                        (V.get handler)
                    in
                    assert (Option.is_none must_be_none) ;
                    let*| stmt =
                      zeko_stmt_of_mina_stmt ~sequencer ~slot_range:!slot_range
                        stmt
                    in
                    Compile_simple.{ prevs = No_prevs; out = stmt } )
              }
            ; { branch_name = "proved-zkapp"
              ; tags =
                  One_tag_sideloaded
                    { sideloaded_tag_name = "proved-zkapp"
                    ; typ = Zkapp_statement.typ
                    ; extract_vk =
                        (fun ({ zkapp_vk; _ } : Zkapp_single_proved_input.t) ->
                          zkapp_vk )
                    }
              ; main =
                  (fun input ->
                    let* { stmt
                         ; handler
                         ; witness
                         ; zkapp_vk
                         ; zkapp_proof
                         ; sequencer
                         ; shift_action_state
                         } =
                      exists Zkapp_single_proved_input.typ
                        ~compute:(V.get input)
                    in
                    let slot_range = ref Slot_range.(constant typ infinite) in
                    let set_slot_range s = slot_range := s in
                    let* zkapp_statement, `Must_verify proof_must_verify =
                      handle_as_prover
                        (fun () ->
                          let@ () = make_checked in
                          Transaction_snark.Base.Zkapp_command_snark.main
                            ?witness:(V.unsafe_unwrap witness)
                            ~zeko_handler:
                              { perform =
                                  (fun eff ->
                                    perform
                                      ~shift_action_states:
                                        [ shift_action_state ] ~set_slot_range
                                      eff )
                              }
                            ~constraint_constants
                            (Transaction_snark.Zkapp_command_segment.Basic
                             .to_single_list Opt_signed_opt_signed )
                            stmt )
                        (V.get handler)
                    in
                    let*| stmt =
                      zeko_stmt_of_mina_stmt ~sequencer ~slot_range:!slot_range
                        stmt
                    in
                    Compile_simple.
                      { prevs =
                          One_prev_sideloaded
                            { public_input = Option.value_exn zkapp_statement
                            ; proof = zkapp_proof
                            ; proof_must_verify
                            ; vk = zkapp_vk
                            }
                      ; out = stmt
                      } )
              }
            ; { branch_name = "merge"
              ; tags = Two_tags_own
              ; main =
                  (fun input ->
                    let* { left =
                             { stmt =
                                 { source_ledger
                                 ; target_ledger = left_target_ledger
                                 ; fee_excess = left_fee_excess
                                 ; sequencer = left_sequencer
                                 ; slot_range = left_slot_range
                                 } as left_stmt
                             ; proof = left_proof
                             }
                         ; right =
                             { stmt =
                                 { source_ledger = right_source_ledger
                                 ; target_ledger
                                 ; fee_excess = right_fee_excess
                                 ; sequencer = right_sequencer
                                 ; slot_range = right_slot_range
                                 } as right_stmt
                             ; proof = right_proof
                             }
                         } =
                      exists Merge_input.typ ~compute:(V.get input)
                    in
                    let* () =
                      Ledger_hash.assert_equal left_target_ledger
                        right_source_ledger
                    in
                    let* fee_excess =
                      Currency.Fee.Signed.Checked.add left_fee_excess
                        right_fee_excess
                    in
                    let* sequencer =
                      assert_equal_safer ~label:__LOC__ PC.typ left_sequencer
                        right_sequencer
                    in
                    let* slot_range_lower =
                      Slot.Checked.(
                        left_slot_range.lower < right_slot_range.lower)
                      >>= if_ ~typ:Slot.typ ~then_:right_slot_range.lower
                            ~else_:left_slot_range.lower
                    in
                    let*| slot_range_upper =
                      Slot.Checked.(
                        left_slot_range.upper < right_slot_range.upper)
                      >>= if_ ~typ:Slot.typ ~then_:left_slot_range.lower
                            ~else_:right_slot_range.lower
                    in
                    Compile_simple.
                      { prevs =
                          Two_prevs
                            ( { public_input = left_stmt
                              ; proof = left_proof
                              ; proof_must_verify = Boolean.true_
                              }
                            , { public_input = right_stmt
                              ; proof = right_proof
                              ; proof_must_verify = Boolean.true_
                              } )
                      ; out =
                          ({ source_ledger
                           ; target_ledger
                           ; fee_excess
                           ; sequencer
                           ; slot_range =
                               { lower = slot_range_lower
                               ; upper = slot_range_upper
                               }
                           } : Zeko_stmt.var)
                      } )
              }
            ]
          () )
