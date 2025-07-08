open Core_kernel
open Mina_base
open Signature_lib
module Field = Snark_params.Tick.Run.Field

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

let inner_public_key =
  let pk =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Field.Constant.of_int 123456789)
  in
  Public_key.compress pk

let inner_account_id =
  Account_id.of_public_key (Public_key.decompress_exn inner_public_key)

let inner_account_index = 0

let indexed_merkle_tree_salt = "indexed merkle tree entry hash"

let indexed_merkle_tree_merge_salt = "indexed merkle tree"

let da_layer_check_salt = "zeko da layer check"

let commit_max_valid_while = Mina_numbers.Global_slot_since_genesis.max_value

module Max_excess_actions = struct
  let inner_sync = Int.pow 2 10

  let commit_inner = Int.pow 2 10

  let commit_outer = Int.pow 2 10
end
