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

let genesis_constants = Genesis_constants.Compiled.genesis_constants

let compile_time_genesis_state =
  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
  in
  compile_time_genesis.data

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
