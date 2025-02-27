open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util

open struct
  module Stack_frame_digest = struct
    include Mina_base.Stack_frame.Digest

    type var = Checked.t
  end

  module Call_stack_digest = struct
    include Mina_base.Call_stack_digest

    type var = Checked.t
  end

  module Account_update_index = struct
    include Mina_numbers.Index

    type var = Checked.t
  end
end

module Local_state = struct
  type t =
    { stack_frame_digest : Stack_frame_digest.t
    ; call_stack_digest : Call_stack_digest.t
    ; transaction_commitment : F.t
    ; full_transaction_commitment : F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Account_update_index.t
    }
  [@@deriving snarky]

  let dummy : var =
    { stack_frame_digest =
        Stack_frame_digest.create Mina_base.Stack_frame.empty
        |> constant Stack_frame_digest.typ
    ; call_stack_digest = Call_stack_digest.(constant empty)
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
    ; source_acc_set : Account_set.t
    ; target_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; accumulated_fees : Currency.Amount.Signed.t
    ; slot_range : Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving snarky]
end

let constraint_constants : Genesis_constants.Constraint_constants.t =
  { sub_windows_per_window = 1
  ; ledger_depth = Account_set.height
  ; work_delay = 1
  ; block_window_duration_ms = 1
  ; transaction_capacity_log_2 = 1
  ; pending_coinbase_depth = 1
  ; coinbase_amount = Currency.Amount.zero
  ; supercharged_coinbase_factor = 1
  ; account_creation_fee = Currency.Fee.of_mina_string_exn "0.1"
  ; fork = None
  }

type update_acc_set_witness =
  { get_account_set_x : unit -> Token_id.t
  ; get_account_set_z : unit -> Token_id.t
  ; get_account_set_x_path : unit -> Account_set.Path.t
  ; get_account_set_y_path : unit -> Account_set.Path.t
  }

open struct
  let derive_token_id ~owner =
    make_checked @@ fun () -> Account_id.Checked.derive_token_id ~owner
end

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
