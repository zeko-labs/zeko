open Core_kernel
open Mina_base

let constraint_constants = Genesis_constants.Constraint_constants.compiled

(* FIXME: Refactor to reduce duplication  *)
let deploy deploy_update ~(fee_payer : Signature_lib.Keypair.t)
    ~(fee_payer_nonce : Account.Nonce.t)
    ~(zkapp_keypair : Signature_lib.Keypair.t) =
  let update =
    Account_update.
      { body =
          { Body.dummy with
            public_key =
              Signature_lib.Public_key.compress zkapp_keypair.public_key
          ; update = deploy_update
          ; use_full_commitment = true
          ; authorization_kind = Signature
          }
      ; authorization = Signature Signature.dummy
      }
  in
  let fee =
    Option.value_exn
    @@ Currency.Fee.(
         constraint_constants.account_creation_fee + of_mina_int_exn 1)
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key =
                Signature_lib.Public_key.compress fee_payer.public_key
            ; fee
            ; valid_until = None
            ; nonce = fee_payer_nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates =
        Zkapp_command.Call_forest.accumulate_hashes'
        @@ Zkapp_command.Call_forest.of_account_updates
             ~account_update_depth:(fun _ -> 0)
             [ update ]
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
  let fee_payer_signature =
    Signature_lib.Schnorr.Chunked.sign
      ~signature_kind:Mina_signature_kind.Testnet fee_payer.private_key
      (Random_oracle.Input.Chunked.field full_commitment)
  in
  let zkapp_signature =
    Signature_lib.Schnorr.Chunked.sign
      ~signature_kind:Mina_signature_kind.Testnet zkapp_keypair.private_key
      (Random_oracle.Input.Chunked.field full_commitment)
  in
  let command =
    { command with
      fee_payer = { command.fee_payer with authorization = fee_payer_signature }
    ; account_updates =
        Zkapp_command.Call_forest.accumulate_hashes'
        @@ Zkapp_command.Call_forest.of_account_updates
             ~account_update_depth:(fun _ -> 0)
             [ { update with authorization = Control.Signature zkapp_signature }
             ]
    }
  in
  command
