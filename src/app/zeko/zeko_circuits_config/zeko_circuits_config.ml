open Core_kernel
open Signature_lib
open Mina_base
open Mina_numbers

module Mina_signature_kind = struct
  type t = Mina_signature_kind.t = Testnet | Mainnet | Other_network of string
  [@@deriving yojson]
end

type t =
  { chain_l1 : Mina_signature_kind.t
  ; chain_l2 : Mina_signature_kind.t
  ; max_valid_while_size : int
  ; holder_accounts_l1 : Public_key.Compressed.t list
  ; holder_account_l2 : Public_key.Compressed.t
  ; helper_token_owner_l1 : Public_key.Compressed.t
  ; zeko_l1 : Public_key.Compressed.t
  ; withdrawal_delay : Global_slot_span.t
  }
[@@deriving yojson]

let t =
  let path = Sys.getenv "ZEKO_CIRCUITS_CONFIG" in
  match Yojson.Safe.from_file path |> of_yojson with
  | Ok t ->
      t
  | Error err ->
      failwithf "Failed to parse Zeko circuits config: %s" err ()

module Inputs = struct
  let inner_public_key = Zeko_constants.inner_public_key

  let chain_l1 = t.chain_l1

  let chain_l2 = t.chain_l2

  let max_valid_while_size = t.max_valid_while_size

  let holder_accounts_l1 = t.holder_accounts_l1

  let holder_account_l2 = t.holder_account_l2

  let helper_token_owner_l1 = t.helper_token_owner_l1

  let zeko_l1 = t.zeko_l1

  let zeko_l2 = Zeko_constants.inner_public_key

  let withdrawal_delay = t.withdrawal_delay

  let holder_account_l1_permissions_enabled : Permissions.t =
    { edit_state = Proof
    ; access = None
    ; send = Proof
    ; receive = None
    ; set_delegate = Impossible
    ; set_permissions = Proof
    ; set_verification_key = (Signature, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Impossible
    ; edit_action_state = Impossible
    ; set_token_symbol = Impossible
    ; increment_nonce = Impossible
    ; set_voting_for = Impossible
    ; set_timing = Impossible
    }

  let holder_account_l1_permissions_disabled : Permissions.t =
    { edit_state = Proof
    ; access = None
    ; send = Impossible
    ; receive = None
    ; set_delegate = Impossible
    ; set_permissions = Proof
    ; set_verification_key = (Signature, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Impossible
    ; edit_action_state = Impossible
    ; set_token_symbol = Impossible
    ; increment_nonce = Impossible
    ; set_voting_for = Impossible
    ; set_timing = Impossible
    }
end
