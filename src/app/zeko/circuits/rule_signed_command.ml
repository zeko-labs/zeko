open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state

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

  let dummy_pc_init = Pending_coinbase.Stack.empty

  let protocol_constants : Genesis_constants.Protocol.t =
    { k = 1
    ; slots_per_epoch = 1000
    ; slots_per_sub_window = 1
    ; grace_period_slots = 1
    ; delta = 1
    ; genesis_state_timestamp = Int64.one
    }

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants ~protocol_constants

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

  let accumulate (f : ('a -> unit) -> 'b Checked.t) : ('b * 'a list) Checked.t =
    let acc = ref [] in
    let running = ref true in
    let*| r =
      f (fun x ->
          (* if this fails it's because you used the generated function after the
             end of its scope, i.e., use-after-free. *)
          assert !running ;
          acc := x :: !acc )
    in
    running := false ;
    (r, !acc)

  let derive_token_id ~owner =
    make_checked @@ fun () -> Account_id.Checked.derive_token_id ~owner
end

type update_acc_set_witness =
  { get_account_set_x : unit -> Token_id.t
  ; get_account_set_z : unit -> Token_id.t
  ; get_account_set_x_path : unit -> Account_set.Path.t
  ; get_account_set_y_path : unit -> Account_set.Path.t
  }

open struct
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
end

module Base_witness = struct
  type t =
    { ledger_path_handler : Handler.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

module Base_witness_V = Mk_V (Base_witness)

module Base_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; witness : Base_witness_V.t
    }
  [@@deriving snarky]
end

let main input =
  let* { source_ledger; source_acc_set; transaction; sequencer; witness } =
    exists Base_input.typ ~compute:(V.get input)
  in
  let* (module Shifted) = Inner_curve.Checked.Shifted.create () in
  let* (target_ledger, fee_excess, _supply_increase), accounts =
    accumulate
    @@ fun set_account_new ->
    Fn.flip handle_as_prover
      As_prover.(
        V.get witness >>| fun { ledger_path_handler; _ } -> ledger_path_handler)
    @@ fun () ->
    Transaction_snark.Base.apply_tagged_transaction ~set_account_new
      ~constraint_constants
      (module Shifted)
      source_ledger Slot.Checked.zero
      (constant Pending_coinbase.Stack.typ dummy_pc_init)
      (constant Pending_coinbase.Stack.typ dummy_pc)
      (constant Pending_coinbase.Stack.typ dummy_pc)
      (constant
         (Mina_state.Protocol_state.Body.typ ~constraint_constants)
         dummy_state_body )
      transaction
  in
  let*| target_acc_set =
    update_acc_set accounts source_acc_set
      ~witness:As_prover.(V.get witness >>| fun x -> x.update_acc_set_witness)
  in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; source_acc_set
    ; target_acc_set
    ; sequencer
    ; accumulated_fees = fee_excess
    ; slot_range = Slot_range.(constant typ infinite)
    ; source_local_state = Local_state.dummy
    ; target_local_state = Local_state.dummy
    }
  in
  Compile_simple.{ prevs = No_prevs; out }
