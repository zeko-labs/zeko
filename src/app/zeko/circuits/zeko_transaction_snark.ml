open Core_kernel
open Zeko_util
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module Zeko_stmt = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; sequencer : Signature_lib.Public_key.Compressed.t
    ; fee_excess : Currency.Fee.Signed.t
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

module Zeko_env = struct
  type t =
    < zeko_mark_shifted_and_get_previous_shiftedness : Account_id.t -> bool >
end

module Zeko_env_V = Mk_V (Zeko_env)
module Witness_V = Mk_V (Transaction_snark.Zkapp_command_segment.Witness)

module Zkapp_single_unproved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : Handler_V.t
    ; witness : Witness_V.t
    ; zeko_env : Zeko_env_V.t
    ; sequencer : PC.t
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
    ; zeko_env : Zeko_env_V.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; sequencer : PC.t
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
  fun ~(sequencer : PC.var) (stmt : Transaction_snark.Statement.With_sok.var) ->
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
    let*| () =
      with_label __LOC__ (fun () ->
          Boolean.(is_neg || is_zero) >>= Boolean.Assert.is_true )
    in
    ( { source_ledger = stmt.source.first_pass_ledger
      ; target_ledger = stmt.target.second_pass_ledger
      ; sequencer
      ; fee_excess = stmt.fee_excess.fee_excess_l
      }
      : Zeko_stmt.var )

let system =
  Compile_simple.compile ~override_wrap_domain:`N1
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
                    Transaction_snark.Base.main ~constraint_constants stmt )
                  (V.get handler)
              in
              let*| stmt = zeko_stmt_of_mina_stmt ~sequencer stmt in
              Compile_simple.{ prevs = No_prevs; out = stmt } )
        }
      ; { branch_name = "single-unproved-zkapp"
        ; tags = No_tags
        ; main =
            (fun input ->
              let* { stmt; handler; witness; zeko_env; sequencer } =
                exists Zkapp_single_unproved_input.typ ~compute:(V.get input)
              in
              let* must_be_none, _must_verify_zkapp =
                handle_as_prover
                  (fun () ->
                    let@ () = make_checked in
                    Transaction_snark.Base.Zkapp_command_snark.main
                      ?witness:(V.unsafe_unwrap witness)
                      ?zeko_env:(V.unsafe_unwrap zeko_env) ~constraint_constants
                      (Transaction_snark.Zkapp_command_segment.Basic
                       .to_single_list Opt_signed )
                      stmt )
                  (V.get handler)
              in
              assert (Option.is_none must_be_none) ;
              let*| stmt = zeko_stmt_of_mina_stmt ~sequencer stmt in
              Compile_simple.{ prevs = No_prevs; out = stmt } )
        }
      ; { branch_name = "double-unproved-zkapp"
        ; tags = No_tags
        ; main =
            (fun input ->
              let* { stmt; handler; witness; zeko_env; sequencer } =
                exists Zkapp_single_unproved_input.typ ~compute:(V.get input)
              in
              let* must_be_none, _must_verify_zkapp =
                handle_as_prover
                  (fun () ->
                    let@ () = make_checked in
                    Transaction_snark.Base.Zkapp_command_snark.main
                      ?witness:(V.unsafe_unwrap witness)
                      ?zeko_env:(V.unsafe_unwrap zeko_env) ~constraint_constants
                      (Transaction_snark.Zkapp_command_segment.Basic
                       .to_single_list Opt_signed_opt_signed )
                      stmt )
                  (V.get handler)
              in
              assert (Option.is_none must_be_none) ;
              let*| stmt = zeko_stmt_of_mina_stmt ~sequencer stmt in
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
                   ; zeko_env
                   ; zkapp_vk
                   ; zkapp_proof
                   ; sequencer
                   } =
                exists Zkapp_single_proved_input.typ ~compute:(V.get input)
              in
              let* zkapp_statement, `Must_verify proof_must_verify =
                handle_as_prover
                  (fun () ->
                    let@ () = make_checked in
                    Transaction_snark.Base.Zkapp_command_snark.main
                      ?witness:(V.unsafe_unwrap witness)
                      ?zeko_env:(V.unsafe_unwrap zeko_env) ~constraint_constants
                      (Transaction_snark.Zkapp_command_segment.Basic
                       .to_single_list Opt_signed_opt_signed )
                      stmt )
                  (V.get handler)
              in
              let*| stmt = zeko_stmt_of_mina_stmt ~sequencer stmt in
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
                           } as left_stmt
                       ; proof = left_proof
                       }
                   ; right =
                       { stmt =
                           { source_ledger = right_source_ledger
                           ; target_ledger
                           ; fee_excess = right_fee_excess
                           ; sequencer = right_sequencer
                           } as right_stmt
                       ; proof = right_proof
                       }
                   } =
                exists Merge_input.typ ~compute:(V.get input)
              in
              let* () =
                Ledger_hash.assert_equal left_target_ledger right_source_ledger
              in
              let* fee_excess =
                Currency.Fee.Signed.Checked.add left_fee_excess right_fee_excess
              in
              let*| sequencer =
                assert_equal_safer ~label:__LOC__ PC.typ left_sequencer
                  right_sequencer
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
                    ({ source_ledger; target_ledger; fee_excess; sequencer } : Zeko_stmt
                                                                               .var)
                } )
        }
      ]
    ()

include (val system)
