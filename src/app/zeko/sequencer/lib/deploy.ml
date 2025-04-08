open Async
open Mina_base
open Account_update
open Signature_lib
module L = Mina_ledger.Ledger

module Z = struct
  open Zeko_circuits

  let proof_permissions : Permissions.t =
    { edit_state = Proof
    ; send = Proof
    ; receive = None
    ; set_delegate = Proof
    ; set_permissions = Proof
    ; set_verification_key = (Either, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Proof
    ; edit_action_state = Proof
    ; set_token_symbol = Proof
    ; increment_nonce = None
    ; set_voting_for = Proof
    ; set_timing = Proof
    ; access = Proof
    }

  let either_permissions : Permissions.t =
    { edit_state = Either
    ; send = Either
    ; receive = None
    ; set_delegate = Either
    ; set_permissions = Either
    ; set_verification_key = (Either, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Either
    ; edit_action_state = Either
    ; set_token_symbol = Either
    ; increment_nonce = Either
    ; set_voting_for = Either
    ; set_timing = Either
    ; access = Either
    }

  module Inner = struct
    let initial_account ?(fake = false) () =
      let%bind vk =
        Compile_simple.Verification_key.of_tag Inner_rules.tag
        |> Promise.to_deferred
      in
      return
        { Account.empty with
          public_key = Zeko_constants.inner_public_key
        ; balance = Currency.Balance.max_int
        ; permissions =
            { (if fake then either_permissions else proof_permissions) with
              access = Permissions.Auth_required.None
            }
        ; zkapp =
            Some
              { Zkapp_account.default with
                app_state =
                  Rollup_state.Inner_state.(value_to_init_state default)
              ; verification_key =
                  Some
                    (Verification_key_wire.Stable.Latest.M.of_binable
                       (Compile_simple.Verification_key.to_pickles vk) )
              }
        }
  end

  module Outer = struct
    let unsafe_deploy ~pause_key ~ledger_hash ~sequencer ~da_key ~acc_set
        ?(fake = false) () =
      let%bind vk =
        Compile_simple.Verification_key.of_tag Outer_rules.tag
        |> Promise.to_deferred
      in
      return
        { Update.dummy with
          app_state =
            Rollup_state.Outer_state.(
              value_to_app_state
                ( { pause_key
                  ; paused = false
                  ; ledger_hash
                  ; inner_action_state =
                      Rollup_state.Inner_action_state.With_length.empty
                  ; sequencer
                  ; da_key
                  ; acc_set
                  }
                  : t ))
        ; verification_key =
            Set
              (Verification_key_wire.Stable.Latest.M.of_binable
                 (Compile_simple.Verification_key.to_pickles vk) )
        ; permissions =
            Set (if fake then either_permissions else proof_permissions)
        }

    let deploy_exn (l : L.t) ~fake =
      if
        not
          (Public_key.Compressed.equal Zeko_constants.inner_public_key
             (L.get_at_index_exn l 0).public_key )
      then failwith "zeko outer deploy: ledger invalid"
      else () ;
      unsafe_deploy ~ledger_hash:(L.merkle_root l) ~fake
  end
end

let deploy_command_exn ?signature_kind ~(signer : Keypair.t)
    ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t) ~(zkapp : Keypair.t)
    ~(initial_ledger : L.t) ~account_set_hash
    ~(account_creation_fee : Currency.Fee.t) ~pause_key ~sequencer ~da_key
    ?(fake = false) () =
  let%bind update =
    Z.Outer.deploy_exn ~pause_key ~sequencer ~da_key ~acc_set:account_set_hash
      ~fake initial_ledger ()
  in
  let zkapp_update =
    { body =
        { Body.dummy with
          public_key = Public_key.compress zkapp.public_key
        ; implicit_account_creation_fee = false
        ; update
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let sender_update =
    { body =
        { Body.dummy with
          public_key = Public_key.compress signer.public_key
        ; balance_change =
            Currency.Amount.(
              account_creation_fee |> of_fee |> Signed.of_unsigned
              |> Signed.negate)
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress signer.public_key
            ; fee
            ; valid_until = None
            ; nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates =
        Zkapp_command.Call_forest.accumulate_hashes
          ~hash_account_update:
            (Zkapp_command.Call_forest.Digest.Account_update.create
               ?chain:signature_kind )
        @@ Zkapp_command.Call_forest.of_account_updates
             ~account_update_depth:(fun _ -> 0)
             [ zkapp_update; sender_update ]
    ; memo = Signed_command_memo.empty
    }
  in
  return (Utils.sign_zkapp_command ?signature_kind command [ zkapp; signer ])
