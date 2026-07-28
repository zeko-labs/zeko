let point_of_string s =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  |> Signature_lib.Public_key.compress

module Inputs = struct
  let inner_public_key = point_of_string "39992"

  let chain_l1 = Mina_signature_kind.Testnet

  let chain_l2 = Mina_signature_kind.Testnet

  let max_valid_while_size = 128

  let holder_accounts_l1 = [ point_of_string "89888" ]

  let ethereum_holder_account_l1 = None

  let multisig_update =
    { Zeko_circuits.Multisig.public_keys = holder_accounts_l1
    ; quorum = Snark_params.Tick.Field.of_int 1
    }

  let multisig_key = multisig_update

  let holder_account_l2 = point_of_string "11111"

  let helper_token_owner_l1 = point_of_string "5123111"

  let zeko_l1 = inner_public_key

  let zeko_l2 = point_of_string "39921"

  let emergency_da_public_key = point_of_string "43210"

  let withdrawal_delay = Mina_numbers.Global_slot_span.of_string "5"

  let bridge_proof_fee = Currency.Amount.zero

  let bridge_fee_recipient_l1 = point_of_string "765431"

  let bridge_fee_recipient_l2 = point_of_string "765432"

  let outer_account_creation_fee = Currency.Fee.zero

  let max_sequencer_inactivity = 128

  let holder_account_l1_permissions_enabled : Mina_base.Permissions.t =
    { edit_state = Proof
    ; access = None
    ; send = Proof
    ; receive = None
    ; set_delegate = Impossible
    ; set_permissions = Proof
    ; set_verification_key =
        (Proof, Mina_numbers.Txn_version.current) (* TODO: correct? *)
    ; set_zkapp_uri = Impossible
    ; edit_action_state = Impossible
    ; set_token_symbol = Impossible
    ; increment_nonce = Impossible
    ; set_voting_for = Impossible
    ; set_timing = Impossible
    }

  let holder_account_l1_permissions_disabled : Mina_base.Permissions.t =
    { edit_state = Proof
    ; access = None
    ; send = Impossible
    ; receive = None
    ; set_delegate = Impossible
    ; set_permissions = Proof
    ; set_verification_key =
        (Proof, Mina_numbers.Txn_version.current) (* TODO: correct? *)
    ; set_zkapp_uri = Impossible
    ; edit_action_state = Impossible
    ; set_token_symbol = Impossible
    ; increment_nonce = Impossible
    ; set_voting_for = Impossible
    ; set_timing = Impossible
    }

  let token_owner_l1 =
    Mina_base.Account_id.create (point_of_string "344213")
      Mina_base.Account_id.Digest.default

  let token_owner_l2 =
    Mina_base.Account_id.create (point_of_string "344213")
      Mina_base.Account_id.Digest.default

  let ethereum_asset_registry_public_key = None

  let ethereum_asset_registry_schema_version =
    Zeko_circuits.Zeko_util.Checked32.zero

  let ethereum_asset_approved_mft_standard_vk_id = Snark_params.Tick.Field.zero

  let ethereum_asset_approved_mft_token_vk_hash = Snark_params.Tick.Field.zero

  let ethereum_asset_approved_mft_admin_vk_hash = Snark_params.Tick.Field.zero

  let ethereum_asset_universal_bridge_vk_id = Snark_params.Tick.Field.zero

  let ethereum_asset_universal_bridge_vk_hash = Snark_params.Tick.Field.zero

  let ethereum_asset_vault_public_key = inner_public_key
end

module Inner_rules_inst = Zeko_circuits.Inner_rules.Make (Inputs) ()

module Outer_rules_inst = Zeko_circuits.Outer_rules.Make (Inputs) ()

let tag = Lazy.force Inner_rules_inst.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "Inner rules vk: %s\n"

let tag = Lazy.force Outer_rules_inst.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "Outer rules vk: %s\n"

module Emergency_da_inst = Zeko_circuits.Emergency_da_rules.Make (Inputs) ()

let tag = Lazy.force Emergency_da_inst.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "Emergency da vk: %s\n"

module B_mina = Zeko_circuits.Bridge_rules.Make_mina (Inputs) ()

let tag = Lazy.force B_mina.System_L1_enabled.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L1 enabled vk: %s\n"

let tag = Lazy.force B_mina.System_L1_disabled.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L1 disabled vk: %s\n"

let tag = Lazy.force B_mina.System_L2.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L2 vk: %s\n"

module B_custom = Zeko_circuits.Bridge_rules.Make_custom (Inputs) ()

let tag = Lazy.force B_custom.System_L1_enabled.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L1 enabled vk: %s\n"

let tag = Lazy.force B_custom.System_L1_disabled.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L1 disabled vk: %s\n"

let tag = Lazy.force B_custom.System_L2.tag

let () =
  Promise.block_on_async_exn (fun () ->
      Compile_simple.Verification_key.of_tag tag )
  |> Compile_simple.Verification_key.hash |> Snark_params.Tick.Field.to_string
  |> Core.printf "System L2 vk: %s\n"
