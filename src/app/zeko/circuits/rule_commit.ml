open Core_kernel
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Mina_base
open Outer

module ASE = Action_state_extension.Make (struct
  let get_iterations = Int.pow 2 14
end)

module Make (Inputs : sig
  val max_valid_while_size : int
end)
(T : Transaction_snark.S) =
struct
  open Inputs

  module PathElt = struct
    type t = { right_side : F.t } [@@deriving snarky]
  end

  module Path =
    SnarkList
      (PathElt)
      (struct
        let length = constraint_constants.ledger_depth
      end)

  module Transaction_snark_V = MkV (Transaction_snark)

  module Witness = struct
    type t =
      { txn_snark : Transaction_snark_V.t
            (** The ledger transition we are performing. *)
      ; public_key : PC.t  (** Our public key on the L2 *)
      ; vk_hash : F.t  (** Our vk hash *)
      ; sequencer : PC.t  (** Sequencer public key *)
      ; slot_range : Slot_range.t  (** slot_range *)
      ; action_state_extension : ASE.t
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t  (** Withdrawals to be processed this time *)
      ; new_inner_acc_path : Path.t
      }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  type extract_txn_snark_result =
    { source_ledger : Ledger_hash.var; target_ledger : Ledger_hash.var }

  let extract_txn_snark :
         Transaction_snark.Statement.With_sok.var
      -> extract_txn_snark_result Checked.t =
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
    fun stmt ->
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

      (* We don't check fee_excess because it's up to the sequencer what they do with it. *)
      (* The supply however must not increase. *)
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
      { source_ledger = stmt.source.first_pass_ledger
      ; target_ledger = stmt.target.second_pass_ledger
      }

  let implied_root (account : Account.var) (path : Path.var) : F.var Checked.t =
    let* init = Account.Checked.digest account in
    Checked.List.foldi path ~init ~f:(fun height acc PathElt.{ right_side } ->
        Ledger_hash.merge_var ~height acc right_side |> Checked.return )

  let get_zkapp (a : Account.var) : Zkapp_account.Checked.t Checked.t =
    let hash, content = a.zkapp in
    let* content =
      exists Zkapp_account.typ
        ~compute:
          (let+| content = As_prover.Ref.get content in
           Option.value ~default:Zkapp_account.default content )
    in
    let*| () =
      with_label __LOC__ (fun () ->
          Field.Checked.Assert.equal hash
          @@ Zkapp_account.Checked.digest content )
    in
    content

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let* ({ txn_snark
          ; public_key
          ; vk_hash
          ; sequencer
          ; slot_range
          ; action_state_extension
          ; old_inner_acc
          ; old_inner_acc_path
          ; new_inner_acc
          ; new_inner_acc_path
          } :
           Witness.var ) =
      exists_witness
    in

    let* () =
      assert_var __LOC__ (fun () ->
          let* diff = Slot.Checked.diff slot_range.upper slot_range.lower in
          Mina_numbers.Global_slot_span.Checked.(
            diff
            < constant
                (Global_slot_span (Unsigned.UInt32.of_int max_valid_while_size))) )
    in

    let* implied_root_old = implied_root old_inner_acc old_inner_acc_path in
    let* implied_root_new = implied_root new_inner_acc new_inner_acc_path in

    let* txn_snark_stmt =
      exists Transaction_snark.Statement.With_sok.typ
        ~compute:
          As_prover.(V.get txn_snark >>| Transaction_snark.statement_with_sok)
    in

    let* { source_ledger; target_ledger } = extract_txn_snark txn_snark_stmt in

    (* We check that the paths provided for the inner account are correct. *)
    let* () =
      with_label __LOC__ (fun () ->
          Field.Checked.Assert.equal
            (Ledger_hash.var_to_hash_packed source_ledger)
            implied_root_old )
    in
    let* () =
      with_label __LOC__ (fun () ->
          Field.Checked.Assert.equal
            (Ledger_hash.var_to_hash_packed target_ledger)
            implied_root_new )
    in

    (* We check that we're dealing with the correct account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal old_inner_acc.public_key
            (constant PC.typ Inner.public_key) )
    in
    (* We repeat the above check for the new account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal new_inner_acc.public_key
          @@ constant PC.typ Inner.public_key )
    in
    let* old_inner_zkapp = get_zkapp old_inner_acc in
    let* new_inner_zkapp = get_zkapp new_inner_acc in

    let outer_action_state_in_inner =
      Outer.Action.State.of_field_var
        (Inner.State.var_of_app_state new_inner_zkapp.app_state)
          .outer_action_state
    in

    let* ( Action_state_extension.Stmt.
             { source = outer_action_state_in_inner'
             ; target = outer_action_state
             }
         , verify_action_state_extension ) =
      ASE.get action_state_extension
    in
    let outer_action_state =
      Action.State.of_field_var outer_action_state.action_state
    in

    (* We want to transfer only deposits finalised with some certainty.
       By submitting `delay_extension` we can prove that we are transfering older deposits. *)
    let* () =
      with_label __LOC__ (fun () ->
          Action.State.to_field_var outer_action_state_in_inner
          |> Field.Checked.Assert.equal
               outer_action_state_in_inner'.action_state )
    in

    (* Withdrawals are registered in the inner account's action state *)
    let old_inner_action_state =
      match old_inner_zkapp.action_state with
      | x :: _ ->
          Inner.Action.State.of_field_var x
    in
    let inner_action_state =
      match new_inner_zkapp.action_state with
      | x :: _ ->
          Inner.Action.State.of_field_var x
    in

    (* Finalize update  *)
    let update =
      { default_account_update.update with
        app_state =
          State.(
            var_to_app_state typ
              ( { ledger_hash = target_ledger; inner_action_state; sequencer }
                : var ))
      }
    in
    let preconditions =
      { default_account_update.preconditions with
        account =
          { default_account_update.preconditions.account with
            state =
              State.to_precondition
                { ledger_hash = Some source_ledger
                ; inner_action_state = Some old_inner_action_state
                ; sequencer = Some sequencer
                }
          ; action_state =
              Or_ignore.Checked.make_unsafe Boolean.true_
                (Action.State.to_field_var outer_action_state)
              (* Our action state must match *)
          }
      ; valid_while = Slot_range.Checked.to_valid_while slot_range
      }
    in
    let* actions =
      Action.commit_to_actions_var
        Action.Commit.
          { ledger = target_ledger
          ; inner_action_state =
              Inner.Action.State.to_field_var inner_action_state
          ; synchronized_outer_action_state =
              Action.State.to_field_var outer_action_state
          ; sequencer
          ; slot_range
          }
    in
    (* Our account update is assembled, specifying our state update, our preconditions, our pk, and our authorization *)
    let account_update =
      { default_account_update with
        public_key
      ; actions
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update
      ; preconditions
      }
    in

    let sequencer_account_update =
      { default_account_update with
        public_key = sequencer
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment = Boolean.true_
      }
    in

    (* Assemble some stuff to help the prover and calculate public output *)
    let* public_output, auxiliary_output =
      make_outputs account_update [ (sequencer_account_update, []) ]
    in
    let* auxiliary_output = V.create auxiliary_output in
    let*| proof =
      As_prover.(V.get txn_snark >>| Transaction_snark.proof)
      |> As_prover.Ref.create
    in
    Pickles.Inductive_rule.
      { previous_proof_statements =
          [ { public_input = txn_snark_stmt
            ; proof_must_verify = Boolean.true_
            ; proof
            }
            (* Proof for Wrapper showing there is a valid transition from source to target *)
          ; verify_action_state_extension
            (* Proof that deposits as recorded on L1 went forward, otherwise it could go backwards,
               and proof that the action_state precondition is an extension of our new all_deposits *)
          ]
      ; public_output
      ; auxiliary_output
      }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup step"
    ; prevs = [ T.tag; force Action_state_extension.tag ]
    ; main = (fun x -> main x |> Run.run_checked)
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end
