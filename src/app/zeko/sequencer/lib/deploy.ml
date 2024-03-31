open Mina_base
open Account_update
open Signature_lib
module L = Mina_ledger.Ledger

let deploy_command_exn ~(signer : Keypair.t) ~(fee : Currency.Fee.t)
    ~(nonce : Account.Nonce.t) ~(zkapp : Keypair.t) ~(initial_ledger : L.t)
    ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    (module Z : Zkapps_rollup.S) : Zkapp_command.t =
  let zkapp_update =
    { body =
        { Body.dummy with
          public_key = Public_key.compress zkapp.public_key
        ; implicit_account_creation_fee = false
        ; update = Z.Outer.deploy_exn initial_ledger
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
              constraint_constants.account_creation_fee |> of_fee
              |> Signed.of_unsigned |> Signed.negate)
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
        Zkapp_command.Call_forest.accumulate_hashes'
        @@ Zkapp_command.Call_forest.of_account_updates
             ~account_update_depth:(fun _ -> 0)
             [ zkapp_update; sender_update ]
    ; memo = Signed_command_memo.empty
    }
  in
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete
      (Zkapp_command.commitment command)
      ~memo_hash:(Signed_command_memo.hash command.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create
           (Account_update.of_fee_payer command.fee_payer) )
  in
  let sender_signature =
    Schnorr.Chunked.sign ~signature_kind:Mina_signature_kind.Testnet
      signer.private_key
      (Random_oracle.Input.Chunked.field full_commitment)
  in
  let zkapp_signature =
    Schnorr.Chunked.sign ~signature_kind:Mina_signature_kind.Testnet
      zkapp.private_key
      (Random_oracle.Input.Chunked.field full_commitment)
  in
  { command with
    fee_payer = { command.fee_payer with authorization = sender_signature }
  ; account_updates =
      Zkapp_command.Call_forest.accumulate_hashes ~hash_account_update:(fun p ->
          Zkapp_command.Digest.Account_update.create p )
      @@ Zkapp_command.Call_forest.of_account_updates
           ~account_update_depth:(fun _ -> 0)
           [ { zkapp_update with
               authorization = Control.Signature zkapp_signature
             }
           ; { sender_update with
               authorization = Control.Signature sender_signature
             }
           ]
  }
