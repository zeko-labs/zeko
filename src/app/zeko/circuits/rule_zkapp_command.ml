open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state
open Checked.Let_syntax

type update_acc_set_witness =
  { get_account_set_x : unit -> Token_id.t
  ; get_account_set_z : unit -> Token_id.t
  ; get_account_set_x_path : unit -> Account_set.Path.t
  ; get_account_set_y_path : unit -> Account_set.Path.t
  }

open struct
  let constraint_constants : Genesis_constants.Constraint_constants.t =
    { sub_windows_per_window = 1
    ; ledger_depth = 35
    ; work_delay = 1
    ; block_window_duration_ms = 1
    ; transaction_capacity_log_2 = 1
    ; pending_coinbase_depth = 1
    ; coinbase_amount = Currency.Amount.zero
    ; supercharged_coinbase_factor = 1
    ; account_creation_fee = Currency.Fee.of_mina_string_exn "0.1"
    ; fork = None
    }

  module Verification_key = struct
    include Pickles.Side_loaded.Verification_key

    type var = Checked.t
  end

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
          let check _failure b = Run.Boolean.Assert.is_true b in
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

  let derive_token_id ~owner =
    make_checked @@ fun () -> Account_id.Checked.derive_token_id ~owner

  let update_acc_set accounts init ~witness =
    Checked.List.fold accounts ~init
      ~f:(fun set (account_id, is_empty_and_writeable) ->
        let open As_prover in
        let* x =
          exists Token_id.typ
            ~compute:(witness >>| fun x -> x.get_account_set_x ())
        in
        let* path_x =
          exists Account_set.Path.typ
            ~compute:(witness >>| fun x -> x.get_account_set_x_path ())
        in
        let* path_y =
          exists Account_set.Path.typ
            ~compute:(witness >>| fun x -> x.get_account_set_y_path ())
        in
        let* z =
          exists Token_id.typ
            ~compute:(witness >>| fun x -> x.get_account_set_z ())
        in
        let* y = derive_token_id ~owner:account_id in
        let* `Before_adding_y set', `After_adding_y new_set =
          Account_set.add_key_var ~x ~path_x ~y ~path_y ~z
            ~check:is_empty_and_writeable ()
        in
        let*| () = assert_equal ~label:__LOC__ Account_set.typ set set' in
        new_set )

  let ( <*> ) : ('a -> 'b) Checked.t -> 'a Checked.t -> 'b Checked.t =
   fun f x ->
    let* f in
    let*| x in
    f x

  let ( <$> ) : ('a -> 'b) -> 'a Checked.t -> 'b Checked.t =
   fun f x ->
    let*| x in
    f x

  type local_state_var =
    ( Transaction_snark.Base.Zkapp_command_snark.stack_frame_t
    , Transaction_snark.Base.Zkapp_command_snark.call_stack_t
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
    ; account_updates_when_start :
        Mina_base.Zkapp_command.Call_forest.With_hashes.t
    }
end

open struct
  module Zkapp_rule_input_witness_V = Mk_V (Zkapp_rule_input_witness)
end

module Zkapp_rule_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; sequencer : Even_PC.t
    ; source_acc_set : Account_set.t
    ; account_updates_when_start : F.t
    ; memo_hash_when_start : F.t
    ; witness : Zkapp_rule_input_witness_V.t
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

module Zkapp_single_proved_input = struct
  type t =
    { base : Zkapp_rule_input.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

let single_unproved ~shift_action_state ~is_start
    Zkapp_rule_input.
      { source_ledger
      ; source_local_state
      ; sequencer
      ; source_acc_set
      ; witness
      ; account_updates_when_start
      ; memo_hash_when_start
      } =
  let witness_p =
    Prover_value.create @@ fun () -> V.unsafe_unwrap witness |> Option.value_exn
  in
  let source_ledger_sparse =
    Prover_value.map ~f:(fun x -> x.source_ledger_sparse) witness_p
  in
  let stack_frame = Prover_value.map ~f:(fun x -> x.stack_frame) witness_p in
  let* ( ( ((g : Transaction_snark.Base.Zkapp_command_snark.Global_state.t), l)
         , accounts_new )
       , slot_ranges ) =
    accumulate
    @@ fun set_slot_range ->
    accumulate
    @@ fun set_account_new ->
    make_checked
    @@ fun () ->
    let module Inputs =
    Transaction_snark.Base.Zkapp_command_snark.Single (struct
      let constraint_constants = constraint_constants

      let spec : Transaction_snark.Zkapp_command_segment.Spec.single =
        { auth_type = Signature; is_start }

      let set_zkapp_input _ = failwith "impossible"

      let set_must_verify _ = failwith "impossible"
    end) in
    let module Logic =
      Mina_transaction_logic.Zkapp_command_logic.Make (Inputs.Inputs) in
    let T = Inputs.Inputs.call_forest_type_eq in
    let T = Inputs.Inputs.call_stack_type_eq in
    let T = Inputs.Inputs.transaction_commitment_type_eq in
    let epoch_data : Epoch_data.var =
      { ledger =
          { hash = Ledger_hash.(constant typ empty_hash)
          ; total_currency = Currency.Amount.(constant typ zero)
          }
      ; seed = Epoch_seed.var_of_hash_packed (constant F.typ Field.zero)
      ; start_checkpoint =
          State_hash.var_of_hash_packed (constant F.typ Field.zero)
      ; lock_checkpoint =
          State_hash.var_of_hash_packed (constant F.typ Field.zero)
      ; epoch_length = Mina_numbers.Length.(constant typ zero)
      }
    in
    let l : Inputs.Inputs.Local_state.t =
      { ledger = (source_ledger, source_ledger_sparse)
      ; stack_frame =
          Inputs.Inputs.stack_frame_unhash source_local_state.stack_frame_digest
            stack_frame
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
    let ( (g : Transaction_snark.Base.Zkapp_command_snark.Global_state.t)
        , (l : local_state_var) ) =
      Logic.apply ~constraint_constants
        ~is_start:
          (`Compute
            { account_updates =
                With_hash.
                  { hash =
                      (Obj.magic (account_updates_when_start : Field.Var.t) : Zkapp_call_forest
                                                                              .Checked
                                                                              .F
                                                                              .t)
                      (* horrible hack *)
                  ; data =
                      Prover_value.map
                        ~f:(fun x -> x.account_updates_when_start)
                        witness_p
                  }
            ; memo_hash = memo_hash_when_start
            ; will_succeed = Boolean.true_
            } )
        { perform =
            (fun x ->
              perform ~shift_action_states:[ shift_action_state ]
                ~set_slot_range ~set_account_new x )
        }
        ( { first_pass_ledger = (source_ledger, source_ledger_sparse)
          ; second_pass_ledger = (source_ledger, source_ledger_sparse)
          ; fee_excess = Currency.Amount.Signed.(constant typ zero)
          ; supply_increase = Currency.Amount.Signed.(constant typ zero)
          ; protocol_state =
              ({ snarked_ledger_hash = Ledger_hash.(constant typ empty_hash)
               ; blockchain_length = Mina_numbers.Length.(constant typ zero)
               ; min_window_density = Mina_numbers.Length.(constant typ zero)
               ; total_currency = Currency.Amount.(constant typ zero)
               ; global_slot_since_genesis =
                   Mina_numbers.Global_slot_since_genesis.(constant typ zero)
               ; staking_epoch_data = epoch_data
               ; next_epoch_data = epoch_data
               } : Zkapp_precondition.Protocol_state.View.Checked.t)
          ; block_global_slot =
              Mina_numbers.Global_slot_since_genesis.(constant typ zero)
          }
        , l )
    in
    (g, l)
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
            ~f:(fun (x : Zkapp_rule_input_witness.t) -> x.update_acc_set_witness)
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
  let* target_ledger, isnt_target_ledger =
    Checked.List.fold
      [ l.ledger; g.first_pass_ledger; g.second_pass_ledger ]
      ~init:(Ledger_hash.(constant typ empty_hash), Boolean.true_)
      ~f:(fun (maybe_target_ledger, isnt_target_ledger) (ledger, _) ->
        let* isnt_target_ledger' =
          Boolean.( || )
          <$> Ledger_hash.equal_var ledger source_ledger
          <*> Ledger_hash.equal_var ledger Ledger_hash.(constant typ empty_hash)
        in
        let* isnt_target_ledger' in
        let* () =
          Boolean.( || ) isnt_target_ledger isnt_target_ledger'
          >>| Boolean.not >>= Boolean.Assert.is_true
        in
        let* next_ledger =
          Ledger_hash.if_ isnt_target_ledger ~then_:ledger
            ~else_:maybe_target_ledger
        in
        let*| next_isnt_target_ledger =
          Boolean.( && ) isnt_target_ledger isnt_target_ledger'
        in
        (next_ledger, next_isnt_target_ledger) )
  in
  let*| () = Boolean.not isnt_target_ledger |> Boolean.Assert.is_true in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; sequencer
    ; accumulated_fees = g.fee_excess
    ; slot_range
    ; source_local_state
    ; target_local_state =
        { transaction_commitment = l.transaction_commitment
        ; full_transaction_commitment = l.transaction_commitment
        ; account_update_index = l.account_update_index
        ; stack_frame_digest = Inputs.Inputs.stack_frame_hash l.stack_frame
        ; call_stack_digest = l.call_stack.hash
        ; excess = l.excess
        }
    ; source_acc_set
    ; target_acc_set
    }
  in
  { Compile_simple.out; prevs = No_prevs }
