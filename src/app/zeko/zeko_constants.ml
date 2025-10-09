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

let inner_holder_key =
  let pk =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Field.Constant.of_int 987654321)
  in
  Public_key.compress pk

let inner_account_index = 0

let indexed_merkle_tree_salt = "indexed merkle tree entry hash"

let indexed_merkle_tree_merge_salt = "indexed merkle tree"

let da_layer_check_salt = "zeko da layer check"

let deposit_salt = "Deposit_params - qFB3jXP*)"

let withdrawal_salt = "Withdrawal_params - qFB3jXP*)"

let bridge_prover_cache = "bridge prover cache"

module Max_excess_actions = struct
  module Inner_sync = struct
    let outer = Int.pow 2 9
  end

  module Commit = struct
    let inner = Int.pow 2 9

    let outer = Int.pow 2 9
  end

  module Finalize_cancelled_deposit = struct
    let outer = Int.pow 2 3

    let outer_with_length = Int.pow 2 3

    let check_accepted = Int.pow 2 2
  end

  module Finalize_deposit = struct
    let outer = Int.pow 2 7

    let check_accepted = Int.pow 2 6
  end

  module Finalize_withdrawal = struct
    let inner = Int.pow 2 9

    let outer = Int.pow 2 9
  end
end

module type FOLDER_ITERATIONS = sig
  val leaf_iterations : int

  val leaf_option_iterations : int

  val extend_iterations : int

  val extend_option_iterations : int
end

module Folder_iterations = struct
  module Ase = struct
    module With_length : FOLDER_ITERATIONS = struct
      let leaf_iterations = Int.pow 2 10

      let leaf_option_iterations = Int.pow 2 9

      let extend_iterations = Int.pow 2 9

      let extend_option_iterations = Int.pow 2 8
    end

    module Without_length : FOLDER_ITERATIONS = struct
      let leaf_iterations = Int.pow 2 10

      let leaf_option_iterations = Int.pow 2 9

      let extend_iterations = Int.pow 2 9

      let extend_option_iterations = Int.pow 2 8
    end
  end

  module Check_accepted : FOLDER_ITERATIONS = struct
    let leaf_iterations = Int.pow 2 5

    let leaf_option_iterations = Int.pow 2 4

    let extend_iterations = Int.pow 2 4

    let extend_option_iterations = Int.pow 2 3
  end
end

let multisig_salt = "multisig"

let da_multisig_max_length = 10
