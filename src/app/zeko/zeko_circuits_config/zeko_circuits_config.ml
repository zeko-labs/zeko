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
  ; max_valid_while_size : Zeko_circuits.Zeko_util.Slot.t
  ; multisig_key : Zeko_circuits.Multisig.t
  ; holder_accounts_l1 : Public_key.Compressed.t list
  ; helper_token_owner_l1 : Public_key.Compressed.t
  ; zeko_l1 : Public_key.Compressed.t
  ; emergency_da_public_key : Public_key.Compressed.t
  ; withdrawal_delay : Global_slot_span.t
  ; bridge_fee_recipient_l1 : Public_key.Compressed.t
  ; bridge_fee_recipient_l2 : Public_key.Compressed.t
  }
[@@deriving yojson]

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
      ( { chain_l1 = Testnet
        ; chain_l2 = Testnet
        ; max_valid_while_size = Zeko_circuits.Zeko_util.Slot.max_value
        ; multisig_key =
            { public_keys = List.map holder_accounts_l1 ~f:fst
            ; quorum = Snark_params.Tick.Field.of_int 1
            }
        ; holder_accounts_l1 = List.map holder_accounts_l1 ~f:fst
        ; helper_token_owner_l1 = fst helper_token_owner_l1
        ; zeko_l1 = fst zeko_l1
        ; emergency_da_public_key = fst emergency_da
        ; withdrawal_delay = Global_slot_span.of_int 5
        ; bridge_fee_recipient_l1 = fst bridge_fee_recipient_l1
        ; bridge_fee_recipient_l2 = fst bridge_fee_recipient_l2
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

module Inputs = struct
  let inner_public_key = Zeko_constants.inner_public_key

  let chain_l1 = t.chain_l1

  let chain_l2 = t.chain_l2

  let max_valid_while_size =
    Zeko_circuits.Zeko_util.Slot.to_int t.max_valid_while_size

  let max_sequencer_inactivity = (24 * 60 * 30 / 3) + 5 (* A month in slots *)

  let multisig_key = t.multisig_key

  let holder_accounts_l1 = t.holder_accounts_l1

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
