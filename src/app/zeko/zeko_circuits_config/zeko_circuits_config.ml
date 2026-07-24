open Core_kernel
open Signature_lib
open Mina_base
open Mina_numbers

module Mina_signature_kind = struct
  type t = Mina_signature_kind.t = Testnet | Mainnet | Other_network of string
  [@@deriving yojson]
end

module Ethereum_token = struct
  type t =
    { asset_id : string
    ; ethereum_token_address : string
    ; token_owner_l2 : Public_key.Compressed.t
    ; holder_account_l2 : Public_key.Compressed.t
    }
  [@@deriving yojson]
end

module Ethereum_assets = struct
  type t =
    { registry_public_key : Public_key.Compressed.t
    ; vault_public_key : Public_key.Compressed.t
    ; approved_mft_standard_vk_id : string
    ; universal_bridge_vk_id : string
    }
  [@@deriving yojson]
end

type t =
  { chain_l1 : Mina_signature_kind.t
  ; chain_l2 : Mina_signature_kind.t
  ; max_valid_while_size : Zeko_circuits.Zeko_util.Slot.t
  ; multisig_key : Zeko_circuits.Multisig.t
  ; holder_accounts_l1 : Public_key.Compressed.t list
  ; ethereum_holder_account_l1 : Public_key.Compressed.t option [@default None]
  ; ethereum_token : Ethereum_token.t option [@default None]
  ; ethereum_assets : Ethereum_assets.t option [@default None]
  ; helper_token_owner_l1 : Public_key.Compressed.t
  ; zeko_l1 : Public_key.Compressed.t
  ; emergency_da_public_key : Public_key.Compressed.t
  ; withdrawal_delay : Global_slot_span.t
  ; bridge_fee_recipient_l1 : Public_key.Compressed.t
  ; bridge_fee_recipient_l2 : Public_key.Compressed.t
  ; outer_account_creation_fee : Currency.Fee.t
  }
[@@deriving yojson]

let ethereum_address_to_public_key address =
  let hex =
    String.lowercase address |> String.chop_prefix_if_exists ~prefix:"0x"
  in
  if String.length hex <> 40 then
    failwithf
      "Ethereum bridge address must contain exactly 20 bytes, got %d hex \
       characters"
      (String.length hex) () ;
  let is_hex_digit = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  if not (String.for_all hex ~f:is_hex_digit) then
    failwith "Ethereum bridge address contains a non-hexadecimal character" ;
  if String.for_all hex ~f:(Char.equal '0') then
    failwith "Ethereum bridge address must not be the zero address" ;
  (* The Kimchi field JSON decoder passes hexadecimal values through the
     fixed-width Bigint256 byte decoder.  Ethereum addresses therefore need
     twelve leading zero bytes before they can be interpreted as a Pasta
     field element. *)
  let field_hex = String.make 24 '0' ^ hex in
  let x =
    match Snark_params.Tick.Field.of_yojson (`String ("0x" ^ field_hex)) with
    | Ok x ->
        x
    | Error error ->
        failwithf "Failed to encode Ethereum bridge address: %s" error ()
  in
  ({ x; is_odd = false } : Public_key.Compressed.t)

let normalize_ethereum_address_exn ~label address =
  let hex =
    String.lowercase address |> String.chop_prefix_if_exists ~prefix:"0x"
  in
  let is_hex_digit = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  if String.length hex <> 40 then
    failwithf "%s must contain exactly 20 bytes" label ()
  else if not (String.for_all hex ~f:is_hex_digit) then
    failwithf "%s contains a non-hexadecimal character" label ()
  else if String.for_all hex ~f:(Char.equal '0') then
    failwithf "%s must not be zero" label ()
  else "0x" ^ hex

let with_ethereum_holder_address t address =
  { t with
    ethereum_holder_account_l1 = Some (ethereum_address_to_public_key address)
  }

let normalize_bytes32_exn ~label value =
  let hex =
    String.lowercase value |> String.chop_prefix_if_exists ~prefix:"0x"
  in
  let is_hex_digit = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  if String.length hex <> 64 then
    failwithf "%s must contain exactly 32 bytes" label ()
  else if not (String.for_all hex ~f:is_hex_digit) then
    failwithf "%s contains a non-hexadecimal character" label ()
  else if String.for_all hex ~f:(Char.equal '0') then
    failwithf "%s must not be zero" label ()
  else "0x" ^ hex

let field_of_uint128_hex_exn ~label hex =
  match
    Snark_params.Tick.Field.of_yojson (`String ("0x" ^ String.make 32 '0' ^ hex))
  with
  | Ok field ->
      field
  | Error error ->
      failwithf "Failed to decode %s: %s" label error ()

let asset_id_limbs_exn asset_id =
  let asset_id =
    normalize_bytes32_exn ~label:"Ethereum token asset ID" asset_id
  in
  let hex = String.drop_prefix asset_id 2 in
  ( field_of_uint128_hex_exn ~label:"Ethereum token asset ID high limb"
      (String.prefix hex 32)
  , field_of_uint128_hex_exn ~label:"Ethereum token asset ID low limb"
      (String.drop_prefix hex 32) )

module Deploy = struct
  type t =
    { holder_accounts_l1 : Private_key.t list
    ; helper_token_owner_l1 : Private_key.t
    ; zeko_l1 : Private_key.t
    ; emergency_da : Private_key.t
    ; bridge_fee_recipient_l1 : Private_key.t
    ; bridge_fee_recipient_l2 : Private_key.t
    }
  [@@deriving yojson]
end

let (t, deploy_config) : t * Deploy.t option =
  match Sys.getenv_opt "ZEKO_CIRCUITS_CONFIG" with
  | Some "test" | None ->
      let keypair_of_b58_sk sk =
        let kp =
          Private_key.of_base58_check_exn sk |> Keypair.of_private_key_exn
        in
        (Public_key.compress kp.public_key, kp.private_key)
      in
      let holder_accounts_l1 =
        [ keypair_of_b58_sk
            "EKDkANpuXLT3AYp4ySHoYfVsjfTM8syQeNd6oTSr5KgS7jnFgXQU"
        ; keypair_of_b58_sk
            "EKE9coDZMm84U8whQmm2JibDijKT2Qe1YWN4xMdJzUbTBfwdUzwF"
        ; keypair_of_b58_sk
            "EKFK44pD33YEQUUgSFDvFmt4rHYZxVUDENh4Pz9iRxaSPNBKNXuV"
        ]
      in
      let helper_token_owner_l1 =
        keypair_of_b58_sk "EKFLJEQouWgCQrBKTrMf8EKvRzuJRsd5hKoo6GWJFdjiS1MFn3np"
      in
      let zeko_l1 =
        keypair_of_b58_sk "EKEFFD7uJayycrse8A2ixBR2Wu7cA5GnGS5ydcYNyzhvr1EPPvj8"
      in
      let emergency_da =
        keypair_of_b58_sk "EKE9VtD6g4AgoscJdxBbFak6yfBgnQDHTpyj23CfNFT5BxL51Zin"
      in
      let bridge_fee_recipient_l1 =
        keypair_of_b58_sk "EKESW6sXA3MA3ugGvEoEHPd6gc9NPB9SRWzuW4VqxBqbNU3W4pFX"
      in
      let bridge_fee_recipient_l2 =
        keypair_of_b58_sk "EKESW6sXA3MA3ugGvEoEHPd6gc9NPB9SRWzuW4VqxBqbNU3W4pFX"
      in
      ( { chain_l1 = Mainnet
        ; chain_l2 = Other_network "zeko-testnet"
        ; max_valid_while_size = Zeko_circuits.Zeko_util.Slot.max_value
        ; multisig_key =
            { public_keys = List.map holder_accounts_l1 ~f:fst
            ; quorum = Snark_params.Tick.Field.of_int 1
            }
        ; holder_accounts_l1 = List.map holder_accounts_l1 ~f:fst
        ; ethereum_holder_account_l1 = None
        ; ethereum_token = None
        ; ethereum_assets = None
        ; helper_token_owner_l1 = fst helper_token_owner_l1
        ; zeko_l1 = fst zeko_l1
        ; emergency_da_public_key = fst emergency_da
        ; withdrawal_delay = Global_slot_span.of_int 5
        ; bridge_fee_recipient_l1 = fst bridge_fee_recipient_l1
        ; bridge_fee_recipient_l2 = fst bridge_fee_recipient_l2
        ; outer_account_creation_fee =
            Zeko_constants.constraint_constants.account_creation_fee
        }
      , Some
          { holder_accounts_l1 = List.map holder_accounts_l1 ~f:snd
          ; helper_token_owner_l1 = snd helper_token_owner_l1
          ; zeko_l1 = snd zeko_l1
          ; emergency_da = snd emergency_da
          ; bridge_fee_recipient_l1 = snd bridge_fee_recipient_l1
          ; bridge_fee_recipient_l2 = snd bridge_fee_recipient_l2
          } )
  | Some path -> (
      match Yojson.Safe.from_file path |> of_yojson with
      | Ok t ->
          let deploy_config =
            Option.map (Sys.getenv_opt "ZEKO_DEPLOY_CONFIG") ~f:(fun path ->
                match Yojson.Safe.from_file path |> Deploy.of_yojson with
                | Ok deploy ->
                    deploy
                | Error err ->
                    failwithf "Failed to parse Zeko deploy config: %s" err () )
          in
          (t, deploy_config)
      | Error err ->
          failwithf "Failed to parse Zeko circuits config: %s" err () )

let t =
  match Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_ADDRESS" with
  | None ->
      t
  | Some address ->
      with_ethereum_holder_address t address

module Inputs = struct
  let inner_public_key = Zeko_constants.inner_public_key

  let chain_l1 = t.chain_l1

  let chain_l2 = t.chain_l2

  let max_valid_while_size =
    Zeko_circuits.Zeko_util.Slot.to_int t.max_valid_while_size

  let max_sequencer_inactivity = (24 * 60 * 30 / 3) + 5 (* A month in slots *)

  let multisig_key = t.multisig_key

  let holder_accounts_l1 = t.holder_accounts_l1

  let ethereum_holder_account_l1 = t.ethereum_holder_account_l1

  module Ethereum_token = struct
    let enabled = Option.is_some t.ethereum_token

    let config =
      Option.value t.ethereum_token
        ~default:
          { Ethereum_token.asset_id =
              "0x0000000000000000000000000000000000000000000000000000000000000001"
          ; ethereum_token_address =
              "0x0000000000000000000000000000000000000001"
          ; token_owner_l2 = Zeko_constants.inner_holder_key
          ; holder_account_l2 = Zeko_constants.inner_holder_key
          }

    let () =
      if
        enabled
        && Public_key.Compressed.equal config.token_owner_l2
             config.holder_account_l2
      then failwith "Ethereum token owner and bridge vault must be distinct"

    let asset_id =
      normalize_bytes32_exn ~label:"Ethereum token asset ID" config.asset_id

    let ethereum_token_address =
      normalize_ethereum_address_exn ~label:"Ethereum token address"
        config.ethereum_token_address

    let ethereum_asset_id_high, ethereum_asset_id_low =
      asset_id_limbs_exn asset_id

    let token_owner_l2 =
      Account_id.create config.token_owner_l2 Token_id.default

    let holder_account_l2 = config.holder_account_l2

    let ethereum_holder_account_l1 =
      match (enabled, t.ethereum_holder_account_l1) with
      | true, None ->
          failwith
            "ethereum_token requires ethereum_holder_account_l1 (or \
             ZEKO_ETHEREUM_BRIDGE_ADDRESS)"
      | _, Some holder ->
          holder
      | false, None ->
          Zeko_constants.inner_holder_key
  end

  module Ethereum_assets = struct
    let enabled = Option.is_some t.ethereum_assets

    let config =
      Option.value t.ethereum_assets
        ~default:
          { Ethereum_assets.registry_public_key = Zeko_constants.inner_holder_key
          ; vault_public_key = Zeko_constants.inner_holder_key
          ; approved_mft_standard_vk_id = "1"
          ; universal_bridge_vk_id = "2"
          }

    let registry_public_key = config.registry_public_key

    let registry_schema_version =
      Zeko_circuits.Zeko_util.Checked32.of_int
        Zeko_constants.Ethereum_asset_registry.schema_version

    let approved_mft_standard_vk_id =
      Snark_params.Tick.Field.of_string config.approved_mft_standard_vk_id

    let universal_bridge_vk_id =
      Snark_params.Tick.Field.of_string config.universal_bridge_vk_id

    let vault_public_key = config.vault_public_key

    let ethereum_holder_account_l1 =
      match (enabled, t.ethereum_holder_account_l1) with
      | true, None ->
          failwith
            "ethereum_assets requires ethereum_holder_account_l1 (or \
             ZEKO_ETHEREUM_BRIDGE_ADDRESS)"
      | _, Some holder ->
          holder
      | false, None ->
          Zeko_constants.inner_holder_key
  end

  let ethereum_asset_registry_public_key =
    if Ethereum_assets.enabled then Some Ethereum_assets.registry_public_key
    else None

  let holder_account_l2 = Zeko_constants.inner_holder_key

  let helper_token_owner_l1 = t.helper_token_owner_l1

  let zeko_l1 = t.zeko_l1

  let zeko_l2 = Zeko_constants.inner_public_key

  let emergency_da_public_key = t.emergency_da_public_key

  let withdrawal_delay = t.withdrawal_delay

  let bridge_proof_fee =
    Currency.Amount.(
      of_fee Zeko_constants.constraint_constants.account_creation_fee
      + of_fee Zeko_constants.constraint_constants.account_creation_fee)
    |> Option.value_exn

  let bridge_fee_recipient_l1 = t.bridge_fee_recipient_l1

  let bridge_fee_recipient_l2 = t.bridge_fee_recipient_l2

  let outer_account_creation_fee = t.outer_account_creation_fee

  let holder_account_l1_permissions_enabled : Permissions.t =
    { edit_state = Proof
    ; access = None
    ; send = Proof
    ; receive = None
    ; set_delegate = Impossible
    ; set_permissions = Proof
    ; set_verification_key = (Proof, Mina_numbers.Txn_version.current)
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
    ; set_verification_key = (Proof, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Impossible
    ; edit_action_state = Impossible
    ; set_token_symbol = Impossible
    ; increment_nonce = Impossible
    ; set_voting_for = Impossible
    ; set_timing = Impossible
    }
end
