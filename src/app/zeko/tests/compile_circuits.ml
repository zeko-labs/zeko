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

  let holder_account_l2 = point_of_string "11111"

  let zeko_l1 = inner_public_key

  let zeko_l2 = point_of_string "39921"

  let withdrawal_delay = Mina_numbers.Global_slot_span.of_string "5"

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
end

module Inner_rules_inst = Zeko_circuits.Inner_rules.Make (Inputs) ()

module Outer_rules_inst = Zeko_circuits.Outer_rules.Make (Inputs) ()

let _tag = Inner_rules_inst.tag

let _tag = Outer_rules_inst.tag

module B_mina = Zeko_circuits.Bridge_rules.Make_mina (Inputs) ()

let _tag = B_mina.System_L1_enabled.tag

let _tag = B_mina.System_L1_disabled.tag

let _tag = B_mina.System_L2.tag

module B_custom = Zeko_circuits.Bridge_rules.Make_custom (Inputs) ()

let _tag = B_custom.System_L1_enabled.tag

let _tag = B_custom.System_L1_disabled.tag

let _tag = B_custom.System_L2.tag
