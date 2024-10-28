open Core_kernel
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Mina_base
open Rollup_state

module Ase_outer_inst = Ase.Make_with_length (struct
  module Action_state = Outer_action_state
  module Action = Outer.Action

  let get_iterations = Int.pow 2 14
end)

module Ase_inner_inst = Ase.Make_with_length (struct
  module Action_state = Inner_action_state
  module Action = Inner.Action

  let get_iterations = Int.pow 2 14
end)

module Verify_both_ases = struct
  let main (w : (Ase_outer_inst.t * Ase_inner_inst.t) V.t) =
    let* outer, inner =
      exists ~compute:(V.get w) Typ.(Ase_outer_inst.typ * Ase_inner_inst.typ)
    in
    let* outer, verify_outer = Ase_outer_inst.get outer in
    let*| inner, verify_inner = Ase_inner_inst.get inner in
    Compile_simple.
      { prevs = Two_prevs (verify_outer, verify_inner); out = (outer, inner) }

  let rule : _ Compile_simple.branch =
    { branch_name = "Verify_both_ases"
    ; tags =
        Two_tags
          (Tag (force Ase.tag_with_length), Tag (force Ase.tag_with_length))
    ; main
    }

  let compilation_result =
    lazy
      (let@ () = Promise.block_on_async_exn in
       compile_simple ~name:"Verify_both_ases" ~branches:[ rule ]
         ~out_typ:Typ.(Ase_outer_inst.Stmt.typ * Ase_inner_inst.Stmt.typ)
         () )
end

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
      ; unverified_ase_inner : Ase_inner_inst.Stmt.t
      ; unverified_ase_outer : Ase_outer_inst.Stmt.t
      ; ases_proof : ProofV.t
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t  (** Withdrawals to be processed this time *)
      ; new_inner_acc_path : Path.t
      ; pause_key : PC.t
      }
    [@@deriving snarky]
  end

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

  let%snarkydef_ main (w : Witness.t V.t) =
    let* ({ txn_snark
          ; public_key
          ; vk_hash
          ; sequencer
          ; slot_range
          ; unverified_ase_inner
          ; unverified_ase_outer
          ; ases_proof
          ; old_inner_acc
          ; old_inner_acc_path
          ; new_inner_acc
          ; new_inner_acc_path
          ; pause_key
          } :
           Witness.var ) =
      exists ~compute:(V.get w) Witness.typ
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

    let synchronized_outer_action_state =
      (Inner.State.var_of_app_state new_inner_zkapp.app_state)
        .outer_action_state
    in

    let ase_outer, ase_inner, verify_ases =
      let verify_ases : _ Compile_simple.prev =
        { public_input = (unverified_ase_outer, unverified_ase_inner)
        ; proof_must_verify = Boolean.true_
        ; proof = ases_proof
        }
      in
      let ase_outer = unverified_ase_outer in
      let ase_inner = unverified_ase_inner in
      (ase_outer, ase_inner, verify_ases)
    in

    let Ase_outer_inst.Stmt.
          { source = synchronized_outer_action_state'
          ; target = outer_action_state
          } =
      ase_outer
    in

    let Ase_inner_inst.Stmt.
          { source = old_inner_action_state; target = new_inner_action_state } =
      ase_inner
    in

    (* We want to transfer only deposits finalised with some certainty.
       By submitting `delay_extension` we can prove that we are transfering older deposits. *)
    let* () =
      with_label __LOC__ (fun () ->
          assert_equal Outer_action_state.With_length.typ
            synchronized_outer_action_state synchronized_outer_action_state' )
    in

    (* Withdrawals are registered in the inner account's action state *)
    let old_inner_action_state' =
      match old_inner_zkapp.action_state with
      | x :: _ ->
          Inner_action_state.unsafe_var_of_field x
    in
    let new_inner_action_state' =
      match new_inner_zkapp.action_state with
      | x :: _ ->
          Inner_action_state.unsafe_var_of_field x
    in
    let* () =
      assert_equal Inner_action_state.typ
        (Inner_action_state.With_length.state_var old_inner_action_state)
        old_inner_action_state'
    in
    let* () =
      assert_equal Inner_action_state.typ
        (Inner_action_state.With_length.state_var new_inner_action_state)
        new_inner_action_state'
    in

    (* Finalize update  *)
    let update =
      { default_account_update.update with
        app_state =
          Outer.State.(
            var_to_app_state typ
              ( { ledger_hash = target_ledger
                ; inner_action_state = new_inner_action_state
                ; sequencer
                ; paused = Boolean.false_
                ; pause_key
                }
                : var ))
      }
    in
    let preconditions =
      { default_account_update.preconditions with
        account =
          { default_account_update.preconditions.account with
            state =
              Outer.State.fine
                { ledger_hash = Some source_ledger
                ; inner_action_state =
                    { state =
                        Some
                          (Inner_action_state.With_length.state_var
                             new_inner_action_state )
                    ; length =
                        Some
                          (Inner_action_state.With_length.length_var
                             new_inner_action_state )
                    }
                ; sequencer = Some sequencer
                ; paused = Some Boolean.false_
                ; pause_key = Some pause_key
                }
              |> var_to_precondition_fine
          ; action_state =
              Or_ignore.Checked.make_unsafe Boolean.true_
                (Outer_action_state.With_length.raw_var outer_action_state)
              (* Our action state must match *)
          }
      ; valid_while = Slot_range.Checked.to_valid_while slot_range
      }
    in
    let* actions =
      Outer.Action.commit_to_actions_var
        Outer.Action.Commit.
          { ledger = target_ledger
          ; inner_action_state = new_inner_action_state
          ; synchronized_outer_action_state
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
    let* out = make_outputs account_update [ (sequencer_account_update, []) ] in
    let*| txn_snark_proof =
      As_prover.(V.get txn_snark >>| Transaction_snark.proof) |> V.create
    in
    Compile_simple.
      { prevs =
          Two_prevs
            ( { public_input = txn_snark_stmt
              ; proof_must_verify = Boolean.true_
              ; proof = txn_snark_proof
              }
            , verify_ases )
      ; out
      }

  let rule : _ Compile_simple.branch =
    match force Verify_both_ases.compilation_result with
    | Result { tag; provers = _; tag_length = _ } ->
        { branch_name = "Rollup step"
        ; tags = Two_tags (Tag T.tag, Tag tag)
        ; main
        }
end
