open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Checked.Let_syntax

let constraint_constants = Genesis_constants.Compiled.constraint_constants

module Stack_frame = struct
  include Mina_base.Stack_frame.Digest

  type var = Checked.t
end

module Call_stack = struct
  include Mina_base.Call_stack_digest

  type var = Checked.t
end

module Account_update_index = struct
  include Mina_numbers.Index

  type var = Checked.t
end

module Local_state = struct
  type t =
    { ledger : Ledger_hash.t
    ; stack_frame : Stack_frame.t
    ; call_stack : Call_stack.t
    ; transaction_commitment : F.t
    ; full_transaction_commitment : F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Account_update_index.t
    }
  [@@deriving snarky]

  let to_mina_var ~supply_increase
      { ledger
      ; stack_frame
      ; call_stack
      ; transaction_commitment
      ; full_transaction_commitment
      ; excess
      ; account_update_index
      } : Mina_state.Local_state.Checked.t =
    { stack_frame
    ; call_stack
    ; transaction_commitment
    ; full_transaction_commitment
    ; excess
    ; account_update_index
    ; ledger
    ; supply_increase
    ; failure_status_tbl = ()
    ; will_succeed = Boolean.true_
    ; success = Boolean.true_
    }

  let dummy : var =
    { ledger = Ledger_hash.(constant typ empty_hash)
    ; stack_frame =
        Stack_frame.create Mina_base.Stack_frame.empty
        |> constant Stack_frame.typ
    ; call_stack = Call_stack.(constant empty)
    ; transaction_commitment =
        constant F.typ Zkapp_command.Transaction_commitment.empty
    ; full_transaction_commitment =
        constant F.typ Zkapp_command.Transaction_commitment.empty
    ; excess = Currency.Amount.Signed.(constant typ zero)
    ; account_update_index = Account_update_index.(constant typ zero)
    }
end

module Zeko_stmt = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; sequencer : Signature_lib.Public_key.Compressed.t
    ; fee_excess : Currency.Fee.Signed.t
    ; slot_range : Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving snarky]
end

module T = struct
  type t = { stmt : Zeko_stmt.t; proof : Proof_V.t } [@@deriving snarky]
end

module Handler_V = Mk_V (Handler)

module Base_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; fee_excess : Currency.Fee.Signed.t
    ; sequencer : PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; handler : Handler_V.t
    }
  [@@deriving snarky]
end

module Merge_input = struct
  type t = { left : T.t; right : T.t } [@@deriving snarky]
end

module Witness_V = Mk_V (Transaction_snark.Zkapp_command_segment.Witness)

module Zkapp_rule_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; connecting_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    ; fee_excess : Currency.Fee.Signed.t
    ; supply_decrease : Currency.Amount.t
    ; witness : Witness_V.t
    ; sequencer : PC.t
    }
  [@@deriving snarky]
end

module Zkapp_single_unproved_input = struct
  type t = { base : Zkapp_rule_input.t; shift_action_state : Boolean.t }
  [@@deriving snarky]
end

module Zkapp_double_unproved_input = struct
  type t =
    { base : Zkapp_rule_input.t
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
    { base : Zkapp_rule_input.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

let dummy_pc_init = Pending_coinbase.Stack.empty

let genesis_constants = Genesis_constants.Compiled.genesis_constants

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

let rule_signed_command input =
  let* { source_ledger
       ; target_ledger
       ; fee_excess
       ; transaction = _
       ; sequencer
       ; handler = _
       } =
    exists Base_input.typ ~compute:(V.get input)
  in
  let handler =
    let+| { source_ledger = _
          ; target_ledger = _
          ; fee_excess = _
          ; transaction
          ; sequencer = _
          ; handler
          } =
      V.get input
    in
    let handler (Snarky_backendless.Request.With { request; respond } as r) =
      match request with
      | Transaction_snark.Base.Transaction ->
          respond (Provide transaction)
      | Transaction_snark.Base.State_body ->
          respond (Provide dummy_state_body)
      | Transaction_snark.Base.Init_stack ->
          respond (Provide dummy_pc_init)
      | Transaction_snark.Base.Global_slot ->
          respond (Provide Mina_numbers.Global_slot_since_genesis.zero)
      | _ ->
          handler r
    in
    handler
  in
  let source : _ Mina_state.Registers.t =
    { first_pass_ledger = source_ledger
    ; second_pass_ledger = target_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state = Mina_state.Local_state.(constant typ (dummy ()))
    }
  in
  let target : _ Mina_state.Registers.t =
    { first_pass_ledger = target_ledger
    ; second_pass_ledger = target_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state = Mina_state.Local_state.(constant typ (dummy ()))
    }
  in
  let stmt : Transaction_snark.Statement.With_sok.var =
    { source
    ; target
    ; connecting_ledger_left = target_ledger
    ; connecting_ledger_right = target_ledger
    ; supply_increase = Currency.Amount.Signed.(constant typ zero)
    ; fee_excess =
        { fee_token_l = Token_id.(Checked.constant default)
        ; fee_excess_l = fee_excess
        ; fee_token_r = Token_id.(Checked.constant default)
        ; fee_excess_r = Currency.Fee.Signed.(Checked.constant zero)
        }
    ; sok_digest = Mina_base.Sok_message.Digest.(constant typ default)
    }
  in
  let*| () =
    handle_as_prover
      (fun () -> Transaction_snark.Base.main ~constraint_constants stmt)
      handler
  in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; sequencer
    ; fee_excess
    ; slot_range = Slot_range.(constant typ infinite)
    ; source_local_state = Local_state.dummy
    ; target_local_state = Local_state.dummy
    }
  in
  Compile_simple.{ prevs = No_prevs; out }

let rule_zkapp ~shift_action_states ~spec
    Zkapp_rule_input.
      { source_ledger
      ; target_ledger
      ; connecting_ledger
      ; fee_excess
      ; supply_decrease
      ; source_local_state
      ; target_local_state
      ; witness
      ; sequencer
      } =
  let slot_range = ref Slot_range.(constant typ infinite) in
  let set_slot_range s = slot_range := s in
  let source : _ Mina_state.Registers.t =
    { first_pass_ledger = source_ledger
    ; second_pass_ledger = connecting_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state =
        Local_state.to_mina_var
          ~supply_increase:Currency.Amount.Signed.(constant typ zero)
          source_local_state
    }
  in
  let supply_increase =
    Currency.Amount.Signed.Checked.(of_unsigned supply_decrease |> negate)
  in
  let target : _ Mina_state.Registers.t =
    { first_pass_ledger = connecting_ledger
    ; second_pass_ledger = target_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state = Local_state.to_mina_var ~supply_increase target_local_state
    }
  in
  let stmt : Transaction_snark.Statement.With_sok.var =
    { source
    ; target
    ; connecting_ledger_left = connecting_ledger
    ; connecting_ledger_right = connecting_ledger
    ; supply_increase = Currency.Amount.Signed.(constant typ zero)
    ; fee_excess =
        { fee_token_l = Token_id.(Checked.constant default)
        ; fee_excess_l = fee_excess
        ; fee_token_r = Token_id.(Checked.constant default)
        ; fee_excess_r = Currency.Fee.Signed.(Checked.constant zero)
        }
    ; sok_digest = Mina_base.Sok_message.Digest.(constant typ default)
    }
  in
  let*| zkapp_statement, _must_verify_zkapp =
    let@ () = make_checked in
    Transaction_snark.Base.Zkapp_command_snark.main
      ?witness:(V.unsafe_unwrap witness)
      ~zeko_handler:
        { perform = (fun eff -> perform ~shift_action_states ~set_slot_range eff)
        }
      ~constraint_constants
      (Transaction_snark.Zkapp_command_segment.Basic.to_single_list spec)
      stmt
  in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; sequencer
    ; fee_excess
    ; slot_range = !slot_range
    ; source_local_state
    ; target_local_state
    }
  in
  (zkapp_statement, out)

let rule_merge input =
  let* { left =
           { stmt =
               { source_ledger
               ; target_ledger = left_target_ledger
               ; source_local_state
               ; target_local_state = left_target_local_state
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
               ; source_local_state = right_source_local_state
               ; target_local_state
               ; fee_excess = right_fee_excess
               ; sequencer = right_sequencer
               ; slot_range = right_slot_range
               } as right_stmt
           ; proof = right_proof
           }
       } =
    exists Merge_input.typ ~compute:(V.get input)
  in
  let* () = Ledger_hash.assert_equal left_target_ledger right_source_ledger in
  let* () =
    assert_equal ~label:__LOC__ Local_state.typ left_target_local_state
      right_source_local_state
  in
  let* fee_excess =
    Currency.Fee.Signed.Checked.add left_fee_excess right_fee_excess
  in
  let* sequencer =
    assert_equal_safer ~label:__LOC__ PC.typ left_sequencer right_sequencer
  in
  let* slot_range_lower =
    Slot.Checked.(left_slot_range.lower < right_slot_range.lower)
    >>= if_ ~typ:Slot.typ ~then_:right_slot_range.lower
          ~else_:left_slot_range.lower
  in
  let*| slot_range_upper =
    Slot.Checked.(left_slot_range.upper < right_slot_range.upper)
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
         ; source_local_state
         ; target_local_state
         ; fee_excess
         ; sequencer
         ; slot_range = { lower = slot_range_lower; upper = slot_range_upper }
         } : Zeko_stmt.var)
    }

include
  ( val Compile_simple.compile ~override_wrap_domain:`N1
          ~name:"zeko-transaction-snark" ~out_typ:Zeko_stmt.typ
          ~branches:
            [ { branch_name = "single-signed-command"
              ; tags = No_tags
              ; main = rule_signed_command
              }
            ; { branch_name = "single-unproved-zkapp-command"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { base; shift_action_state } =
                      exists Zkapp_single_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| _, out =
                      rule_zkapp base
                        ~shift_action_states:[ shift_action_state ]
                        ~spec:Opt_signed
                    in
                    Compile_simple.{ prevs = No_prevs; out } )
              }
            ; { branch_name = "double-unproved-zkapp-command"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { base
                         ; shift_action_state_first
                         ; shift_action_state_second
                         } =
                      exists Zkapp_double_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| _, out =
                      rule_zkapp base
                        ~shift_action_states:
                          [ shift_action_state_first
                          ; shift_action_state_second
                          ]
                        ~spec:Opt_signed_opt_signed
                    in
                    Compile_simple.{ prevs = No_prevs; out } )
              }
            ; { branch_name = "single-proved-zkapp-command"
              ; tags =
                  One_tag_sideloaded
                    { sideloaded_tag_name =
                        "single-proved-zkapp-command-sideloaded-vk"
                    ; typ = Zkapp_statement.typ
                    ; extract_vk =
                        (fun ({ zkapp_vk; _ } : Zkapp_single_proved_input.t) ->
                          Compile_simple.Verification_key.of_pickles zkapp_vk )
                    }
              ; main =
                  (fun input ->
                    let* { base; zkapp_vk; zkapp_proof; shift_action_state } =
                      exists Zkapp_single_proved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| zkapp_statement, out =
                      rule_zkapp base
                        ~shift_action_states:[ shift_action_state ] ~spec:Proved
                    in
                    Compile_simple.
                      { prevs =
                          One_prev_sideloaded
                            { public_input = Option.value_exn zkapp_statement
                            ; proof = zkapp_proof
                            ; proof_must_verify = Boolean.true_
                            ; vk =
                                Compile_simple.Verification_key.var_of_pickles
                                  zkapp_vk
                            }
                      ; out
                      } )
              }
            ; { branch_name = "merge"; tags = Two_tags_own; main = rule_merge }
            ]
          () )
