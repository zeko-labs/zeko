open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state
open Checked.Let_syntax

open struct
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

  let perform (type local_state) ~(shift_action_states : Boolean.var list)
      ~(set_slot_range : Slot_range.var -> unit)
      ~(set_global_slot_range : Slot_range.var -> unit)
      ~(set_account_new : Account_id.var * Boolean.var Checked.t -> unit) =
    let shift_action_states = ref shift_action_states in
    fun (type r)
        (eff :
          ( r
          , < bool : Boolean.var
            ; account :
                (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t
            ; local_state : local_state
            ; .. > )
          Mina_transaction_logic.Zkapp_command_logic.Eff.t ) : r ->
      match eff with
      | Check_valid_while_precondition
          ( (valid_while : Zkapp_precondition.Valid_while.Checked.t)
          , _global_state ) ->
          let ({ lower; upper } : _ Zkapp_precondition.Closed_interval.t) =
            Zkapp_basic.Or_ignore.Checked.data valid_while
          in
          (* NB: We don't need to check whether valid_while is Some, because even if it is
             None, nothing will break. It should be (minimum, maximum) in this case.
          *)
          set_slot_range { lower; upper } ;
          (* We always return true because failure doesn't happen here but in the commit rule. *)
          Boolean.true_
      | Check_protocol_state_precondition
          ( ({ global_slot_since_genesis; _ } as protocol_state_predicate :
              Zkapp_precondition.Protocol_state.Checked.t )
          , _global_state ) ->
          Run.run_checked
            Zkapp_precondition.Protocol_state.(
              assert_equal ~label:__LOC__ typ protocol_state_predicate
                { (constant typ accept) with global_slot_since_genesis }) ;
          let ({ lower; upper } : _ Zkapp_precondition.Closed_interval.t) =
            Zkapp_basic.Or_ignore.Checked.data global_slot_since_genesis
          in
          (* Same as above, if it's not set, it ought to be (minimum, maximum) according to
             the typ. *)
          set_global_slot_range { lower; upper } ;
          Boolean.true_
      | Check_account_precondition
          ( ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
          , (account : _ With_hash.t)
          , new_account
          , local_state ) ->
          let check _failure b =
            Run.with_label __LOC__ @@ fun () -> Run.Boolean.Assert.is_true b
          in
          Zkapp_precondition.Account.Checked.check ~new_account ~check
            account_update.data.preconditions.account account.data ;
          local_state
      | Init_account
          { account_update =
              ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
          ; account : ( Account.Checked.Unhashed.t
                      , Field.Var.t lazy_t )
                      With_hash.t
          } ->
          let account_id =
            Account_id.Checked.create account_update.data.public_key
              account_update.data.token_id
          in
          let is_new =
            PC.Checked.equal account.data.public_key PC.(constant typ empty)
          in
          set_account_new (account_id, is_new) ;
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

  type local_state_var =
    ( Transaction_snark.Base.Zkapp_command_snark.zeko_stack_frame_t
    , Transaction_snark.Base.Zkapp_command_snark.zeko_call_stack_t
    , Currency.Amount.Signed.var
    , Ledger_hash.var * Sparse_ledger_base.t Prover_value.t
    , Boolean.var
    , F.var
    , Mina_numbers.Index.Checked.t
    , unit )
    Mina_transaction_logic.Zkapp_command_logic.Local_state.t
end

module Zkapp_rule_input_witness = struct
  open Mina_base

  type t =
    { stack_frame :
        ( Token_id.Stable.V2.t
        , Zkapp_command.Call_forest.With_hashes.Stable.V1.t )
        Stack_frame.Stable.V1.t
    ; call_stack :
        ( ( ( Token_id.Stable.V2.t
            , Zkapp_command.Call_forest.With_hashes.Stable.V1.t )
            Stack_frame.Stable.V1.t
          , Stack_frame.Digest.Stable.V1.t )
          With_hash.t
        , Call_stack_digest.Stable.V1.t )
        With_stack_hash.Stable.V1.t
        list
    ; source_ledger_sparse : Mina_ledger.Sparse_ledger.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

open struct
  module Zkapp_rule_input_witness_V = Mk_V (Zkapp_rule_input_witness)
  module Call_forest_V = Mk_V (Mina_base.Zkapp_command.Call_forest.With_hashes)

  module Zkapp_call_forest_F = struct
    type t = Mina_base.Zkapp_command.Digest.Forest.t

    type var = Mina_base.Zkapp_command.Digest.Forest.Checked.t

    let typ = Mina_base.Zkapp_command.Digest.Forest.typ
  end
end

module Per_account_update = struct
  type t =
    { account_updates : Zkapp_call_forest_F.t
    ; memo_hash : F.t
    ; account_updates_data : Call_forest_V.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

module Zkapp_rule_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; sequencer : Even_PC.t
    ; source_acc_set : Account_set.t
    ; witness : Zkapp_rule_input_witness_V.t
    }
  [@@deriving snarky]
end

open struct
  let shared ~gen_prevs
      Zkapp_rule_input.
        { source_ledger
        ; source_local_state
        ; sequencer
        ; source_acc_set
        ; witness
        }
      (account_updates_data :
        ( Control.Tag.t
        * [ `Compute_in_circuit | `Yes | `No ]
        * Per_account_update.var )
        list ) =
    let witness_p =
      Prover_value.create
      @@ fun () -> V.unsafe_unwrap witness |> Option.value_exn
    in
    let source_ledger_sparse =
      Prover_value.map ~f:(fun x -> x.source_ledger_sparse) witness_p
    in
    let stack_frame = Prover_value.map ~f:(fun x -> x.stack_frame) witness_p in
    let module Global_state = struct
      type t =
        { fee_excess : Currency.Amount.Signed.var
        ; supply_increase : Currency.Amount.Signed.var
        }

      let fee_excess { fee_excess; _ } = fee_excess

      let set_fee_excess t fee_excess = { t with fee_excess }

      let supply_increase { supply_increase; _ } = supply_increase

      let set_supply_increase t supply_increase = { t with supply_increase }

      let block_global_slot _ = constant Slot.typ Slot.zero
    end in
    let* ( ( (((((g, l), vks), must_verify_zkapp), zkapp_input), accounts_new)
           , global_slot_ranges )
         , slot_ranges ) =
      accumulate
      @@ fun set_slot_range ->
      accumulate
      @@ fun set_global_slot_range ->
      accumulate
      @@ fun set_account_new ->
      accumulate
      @@ fun set_zkapp_input ->
      accumulate
      @@ fun set_must_verify_zkapp ->
      accumulate
      @@ fun set_vk ->
      let* stack_frame =
        make_checked
        @@ fun () ->
        Transaction_snark.Base.Zkapp_command_snark.zeko_stack_frame_unhash
          source_local_state.stack_frame_digest stack_frame
      in
      let l : _ Mina_transaction_logic.Zkapp_command_logic.Local_state.t =
        { ledger = (source_ledger, source_ledger_sparse)
        ; stack_frame
        ; call_stack =
            { With_hash.hash = source_local_state.call_stack_digest
            ; data = Prover_value.map ~f:(fun x -> x.call_stack) witness_p
            }
        ; transaction_commitment = source_local_state.transaction_commitment
        ; full_transaction_commitment =
            source_local_state.full_transaction_commitment
        ; excess = source_local_state.excess
        ; supply_increase = Currency.Amount.Signed.(constant typ zero)
        ; will_succeed = Boolean.true_
        ; success = Boolean.true_
        ; account_update_index = source_local_state.account_update_index
        ; failure_status_tbl = ()
        }
      in
      let g : Global_state.t =
        { fee_excess = Currency.Amount.Signed.(constant typ zero)
        ; supply_increase = Currency.Amount.Signed.(constant typ zero)
        }
      in
      Checked.List.fold account_updates_data ~init:(g, l)
        ~f:(fun
             (g, l)
             ( auth_type
             , is_start
             , { account_updates
               ; memo_hash
               ; account_updates_data
               ; shift_action_state
               } )
           ->
          make_checked
          @@ fun () ->
          let module Patched = struct
            module Inst =
            Transaction_snark.Base.Zkapp_command_snark.Single (struct
              let constraint_constants = constraint_constants

              let spec : Transaction_snark.Zkapp_command_segment.Spec.single =
                { auth_type; is_start }

              let set_zkapp_input = set_zkapp_input

              let set_must_verify = set_must_verify_zkapp
            end)

            open struct
              module G = Global_state
            end

            include Inst.Inputs

            module Account = struct
              include Account

              let register_verification_key ({ data = a; _ } : t) =
                Data_as_hash.hash a.zkapp.verification_key.data |> set_vk
            end

            module Global_state = G
          end in
          let module Logic =
            Mina_transaction_logic.Zkapp_command_logic.Make (Patched) in
          let T = Patched.zeko_transaction_commitment_type_eq in
          let T = Patched.zeko_call_forest_type_eq in
          let g, (l : local_state_var) =
            Logic.apply ~constraint_constants
              ~is_start:
                (`Compute
                  { account_updates =
                      With_hash.
                        { hash = account_updates
                        ; data =
                            ( Prover_value.create
                            @@ fun () ->
                            V.unsafe_unwrap account_updates_data
                            |> Option.value_exn )
                        }
                  ; memo_hash
                  ; will_succeed = Boolean.true_
                  } )
              { perform =
                  (fun x ->
                    perform ~shift_action_states:[ shift_action_state ]
                      ~set_slot_range ~set_global_slot_range ~set_account_new x
                    )
              }
              (g, l)
          in
          (g, l) )
    in
    let* accounts_new =
      Checked.List.map accounts_new ~f:(fun (account, is_new) ->
          let*| is_new in
          (account, is_new) )
    in
    let* target_acc_set =
      update_acc_set accounts_new source_acc_set
        ~witness:
          ( V.map
              ~f:(fun (x : Zkapp_rule_input_witness.t) ->
                x.update_acc_set_witness )
              witness
          |> V.get )
    in
    let* slot_range =
      Checked.List.fold ~init:None slot_ranges ~f:(function
        | None ->
            fun x -> Checked.return (Some x)
        | Some x ->
            fun y -> slot_range_intersection x y >>| fun x -> Some x )
      >>| Option.value ~default:Slot_range.(constant typ infinite)
    in
    let* global_slot_range =
      Checked.List.fold ~init:None global_slot_ranges ~f:(function
        | None ->
            fun x -> Checked.return (Some x)
        | Some x ->
            fun y -> slot_range_intersection x y >>| fun x -> Some x )
      >>| Option.value ~default:Slot_range.(constant typ infinite)
    in
    let target_ledger, _ = l.ledger in
    let out : Zeko_stmt.var =
      { source_ledger
      ; target_ledger
      ; sequencer
      ; accumulated_fees = g.fee_excess
      ; slot_range
      ; global_slot_range
      ; source_local_state
      ; target_local_state =
          { transaction_commitment = l.transaction_commitment
          ; full_transaction_commitment = l.full_transaction_commitment
          ; account_update_index = l.account_update_index
          ; stack_frame_digest = force l.stack_frame.hash
          ; call_stack_digest = l.call_stack.hash
          ; excess = l.excess
          }
      ; source_acc_set
      ; target_acc_set
      }
    in
    let*| prevs = gen_prevs vks must_verify_zkapp zkapp_input in
    { Compile_simple.out; prevs }
end

module Zkapp_single_unproved_input = struct
  type t = { base : Zkapp_rule_input.t; first : Per_account_update.t }
  [@@deriving snarky]
end

let single_unproved input =
  let* Zkapp_single_unproved_input.{ base; first } =
    exists Zkapp_single_unproved_input.typ ~compute:(V.get input)
  in
  shared
    ~gen_prevs:(fun _ _ _ -> Checked.return Compile_simple.No_prevs)
    base
    [ (Signature, `Compute_in_circuit, first) ]

module Zkapp_double_unproved_input = struct
  type t =
    { base : Zkapp_rule_input.t
    ; first : Per_account_update.t
    ; second : Per_account_update.t
    }
  [@@deriving snarky]
end

let double_unproved input =
  let* Zkapp_double_unproved_input.{ base; first; second } =
    exists Zkapp_double_unproved_input.typ ~compute:(V.get input)
  in
  shared
    ~gen_prevs:(fun _ _ _ -> Checked.return Compile_simple.No_prevs)
    base
    [ (Signature, `Compute_in_circuit, first)
    ; (Signature, `Compute_in_circuit, second)
    ]

module Zkapp_single_proved_input = struct
  type t =
    { base : Zkapp_rule_input.t
    ; vk : Compile_simple.Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; first : Per_account_update.t
    }
  [@@deriving snarky]
end

let single_proved input =
  let* Zkapp_single_proved_input.{ base; vk; zkapp_proof; first } =
    exists Zkapp_single_proved_input.typ ~compute:(V.get input)
  in
  shared
    ~gen_prevs:(fun vks proof_must_verify_list public_input_list ->
      match (vks, proof_must_verify_list, public_input_list) with
      | [ vk_hash ], [ proof_must_verify ], [ public_input ] ->
          let*| () =
            assert_equal ~label:__LOC__ F.typ
              (Compile_simple.Verification_key.hash_var vk)
              vk_hash
          in
          Compile_simple.One_prev_sideloaded
            { public_input; proof = zkapp_proof; proof_must_verify; vk }
      | _ ->
          failwith "impossible" )
    base
    [ (Proof, `Compute_in_circuit, first) ]
