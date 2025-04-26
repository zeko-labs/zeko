open Core_kernel
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Mina_base
open Rollup_state
open Checked.Let_syntax

(** Used to prove that the synchronized outer action state is a predecessor of the current one. *)
module Ase_outer_inst = Ase.Without_length.Make (struct
  module Action_state = Outer_action_state

  let get_iterations = Zeko_constants.Max_excess_actions.commit_outer
end)

(** Used to prove the length of the inner action state as stored on the outer account. *)
module Ase_inner_inst = Ase.With_length.Make (struct
  module Action_state = Inner_action_state

  let get_iterations = Zeko_constants.Max_excess_actions.commit_inner
end)

(** Proves both Ase_outer_inst and Ase_inner_inst, to circumvent limitation of two recursive proof verifications per proof. *)
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
    ; tags = Two_tags (Ase.Without_length.tag, Ase.With_length.tag)
    ; main
    }

  include
    ( val Compile_simple.compile ~name:"Verify_both_ases" ~branches:[ rule ]
            ~out_typ:Typ.(Ase_outer_inst.Stmt.typ * Ase_inner_inst.Stmt.typ)
            () )
end

module Make (Inputs : sig
  (** max_valid_while_size signifies how big the valid_while can be for commits. *)
  val max_valid_while_size : int

  (** The public key of the inner account *)
  val inner_public_key : PC.t

  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  module PathElt = struct
    type t = { right_side : F.t } [@@deriving snarky]
  end

  (** Witness for path to inner account. Path is implicitly all left. *)
  module Path =
    SnarkList
      (PathElt)
      (struct
        let length =
          Genesis_constants.Compiled.constraint_constants.ledger_depth
      end)

  module Witness = struct
    type t =
      { txn_snark : Txn_rules.t  (** The ledger transition we are performing. *)
      ; public_key : PC.t  (** Our public key on the L2 *)
      ; vk_hash : F.t  (** Our vk hash *)
      ; verify_both_ases : Verify_both_ases.t
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t
      ; new_inner_acc_path : Path.t
      ; da_signature : Signature_lib.Schnorr.Chunked.Signature.t
      ; da_key : Even_PC.t
      ; slot_range : Slot_range.t
      }
    [@@deriving snarky]
  end

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
          ; verify_both_ases
          ; old_inner_acc
          ; old_inner_acc_path
          ; new_inner_acc
          ; new_inner_acc_path
          ; da_signature
          ; da_key
          ; slot_range
          } :
           Witness.var ) =
      exists ~compute:(V.get w) Witness.typ
    in
    (* Calculate the root ledger hashes, to be checked against txn snark. *)
    let* implied_root_old = implied_root old_inner_acc old_inner_acc_path in
    let* implied_root_new = implied_root new_inner_acc new_inner_acc_path in

    let* ( { source_ledger
           ; target_ledger
           ; source_local_state
           ; target_local_state
           ; sequencer
           ; accumulated_fees
           ; slot_range = txn_snark_slot_range
           ; global_slot_range
           ; source_acc_set
           ; target_acc_set
           }
         , verify_txn_snark ) =
      Txn_rules.get txn_snark
    in

    (* The local states must be empty, ensuring that there is no incomplete zkapp transaction being committed. *)
    let* () =
      Txn_state.Local_state.(
        assert_equal ~label:__LOC__ typ source_local_state dummy)
    in
    let* () =
      Txn_state.Local_state.(
        assert_equal ~label:__LOC__ typ target_local_state dummy)
    in

    (* DA check, simply see if public key in question has signed our ledger. *)
    let* () =
      (* TODO: Is this correct? *)
      let* (module Shifted) = Inner_curve.Checked.Shifted.create () in
      let* da_key_uncompressed =
        Even_PC.to_pc_var da_key |> Signature_lib.Public_key.decompress_var
      in
      let input =
        let open Random_oracle.Input.Chunked in
        append
          (Ledger_hash.var_to_field target_ledger |> field)
          (Account_set.to_input_var target_acc_set)
      in
      let* payload =
        make_checked (fun () ->
            Random_oracle.Checked.hash
              ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
              (Random_oracle.Checked.pack_input input) )
      in
      Signature_lib.Schnorr.Chunked.Checked.assert_verifies
        (module Shifted)
        da_signature da_key_uncompressed
        (Random_oracle.Input.Chunked.field payload)
    in

    (* Sequencer must take fees. A non-zero magnitude would
       either mean printing or burning L2 MINA. *)
    let* () =
      Currency.Amount.(
        Signed.Checked.magnitude accumulated_fees
        >>= assert_equal ~label:__LOC__ typ (constant typ zero))
    in

    (* We check that the valid while isn't too big. Do note that the slot_range is inclusive surprisingly,
       so there should be no one-off bug below. In the case where lower and upper are equal,
       max_valid_while_size must be at least 1. *)
    let* () =
      assert_var __LOC__ (fun () ->
          let* diff = Slot.Checked.diff slot_range.upper slot_range.lower in
          Mina_numbers.Global_slot_span.Checked.(
            diff
            < constant
                (Global_slot_span (Unsigned.UInt32.of_int max_valid_while_size))) )
    in

    (* Our slot range must be a subset of the txn snark slot range. *)
    let* () =
      assert_var __LOC__
      @@ fun () -> Slot.Checked.(slot_range.lower >= txn_snark_slot_range.lower)
    in
    let* () =
      assert_var __LOC__
      @@ fun () -> Slot.Checked.(slot_range.upper <= txn_snark_slot_range.upper)
    in

    (* We check that the paths provided for the inner account are correct. *)
    let* source_ledger =
      assert_equal_safer ~label:__LOC__ Ledger_hash.typ source_ledger
        (Ledger_hash.var_of_hash_packed implied_root_old)
    in
    let* target_ledger =
      assert_equal_safer ~label:__LOC__ Ledger_hash.typ target_ledger
        (Ledger_hash.var_of_hash_packed implied_root_new)
    in

    (* We check that we're dealing with the correct account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal old_inner_acc.public_key
            (constant PC.typ inner_public_key) )
    in
    (* We repeat the above check for the new account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal new_inner_acc.public_key
          @@ constant PC.typ inner_public_key )
    in

    (* Extract the zkapp portion of the accounts. *)
    let* old_inner_zkapp = get_zkapp old_inner_acc in
    let* new_inner_zkapp = get_zkapp new_inner_acc in

    (* Extract the outer action state as synchronized to the new inner account. *)
    let synchronized_outer_action_state =
      (Inner_state.var_of_app_state new_inner_zkapp.app_state)
        .outer_action_state
    in

    (* Extract information from Verify_both_ases wrapper proof. *)
    let* (ase_outer, ase_inner), verify_ases =
      Verify_both_ases.get verify_both_ases
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

    (* The sequencer doesn't have to synchronize all actions immediately.
       They can delay it by an arbitrary amount.
       This checks that the source of the ase_outer proof is equal to the
       synchronized outer action state.
       We don't check the lengths here, since it isn't important for this purpose.
    *)
    let* synchronized_outer_action_state =
      let*| () =
        assert_equal ~label:__LOC__ Outer_action_state.typ
          (Outer_action_state.With_length.state_var
             synchronized_outer_action_state )
          synchronized_outer_action_state'
      in
      synchronized_outer_action_state
    in

    (* Extract the inner action states. *)
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
    (* We check that the above values match with what we got from ase_inner. *)
    let* old_inner_action_state =
      let*| () =
        assert_equal Inner_action_state.typ
          (Inner_action_state.With_length.state_var old_inner_action_state)
          old_inner_action_state'
      in
      old_inner_action_state
    in
    let* new_inner_action_state =
      let*| () =
        assert_equal Inner_action_state.typ
          (Inner_action_state.With_length.state_var new_inner_action_state)
          new_inner_action_state'
      in
      new_inner_action_state
    in

    (* Finalize update  *)
    let update =
      { default_account_update.update with
        app_state =
          Outer_state.fine
            { ledger_hash = Some target_ledger
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
                (* The inner action state as recorded now.
                   Other zkapps can match on this, or deduce it
                   directly from `ledger_hash`. There is no real difference
                   currently.
                   However, we also store the length here, which is of importance
                   to many other zkapps.
                *)
            ; sequencer = None (* We don't update the sequencer. *)
            ; paused = None (* We don't pause the rollup. *)
            ; pause_key = None (* We don't update the pause key. *)
            ; da_key = None
            ; acc_set = Some target_acc_set
            }
          |> var_to_app_state_fine
      }
    in
    let preconditions =
      { Account_update.Preconditions.Checked.account =
          { default_account_update.preconditions.account with
            state =
              Outer_state.fine
                { ledger_hash =
                    Some source_ledger
                    (* The original state of the rollup ledger. *)
                ; inner_action_state =
                    { state =
                        Some
                          (Inner_action_state.With_length.state_var
                             old_inner_action_state )
                    ; length =
                        Some
                          (Inner_action_state.With_length.length_var
                             old_inner_action_state )
                    }
                    (* The inner action state as recorded before.
                       The state we already know from the ledger hash,
                       but the length is information we didn't have before.
                    *)
                ; sequencer = Some sequencer (* We must be the sequencer. *)
                ; paused = Some Boolean.false_ (* We must not be paused. *)
                ; pause_key =
                    None (* We don't care about who can pause the rollup. *)
                ; da_key = Some da_key
                ; acc_set = Some source_acc_set
                }
              |> var_to_precondition_fine
          ; action_state =
              Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                (Outer_action_state.raw_var outer_action_state)
              (* Our action state must match *)
          }
      ; valid_while = Slot_range.Checked.to_valid_while slot_range
      ; network =
          { default_account_update.preconditions.network with
            global_slot_since_genesis =
              Slot_range.Checked.to_valid_while global_slot_range
          }
      }
    in
    (* We submit an action that summarizes what we did. Used as a way to timestamp when actions were synchronized. *)
    let* actions =
      Outer_action.commit_to_actions_var
        Outer_action.Commit.
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
        public_key = Even_PC.to_pc_var sequencer
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment = Boolean.true_
      }
    in

    (* Assemble some stuff to help the prover and calculate public output *)
    let*| out =
      make_outputs ~chain:chain_l1 account_update
        [ (sequencer_account_update, []) ]
    in
    Compile_simple.{ prevs = Two_prevs (verify_txn_snark, verify_ases); out }

  let rule : _ Compile_simple.branch =
    { branch_name = "Rollup step"
    ; tags = Two_tags (Txn_rules.tag, Verify_both_ases.tag)
    ; main
    }
end
