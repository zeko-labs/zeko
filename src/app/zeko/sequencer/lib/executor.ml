open Async
open Async_kernel
open Core_kernel
open Mina_base
open Signature_lib

type t =
  { l1_uri : Uri.t Cli_lib.Flag.Types.with_name
  ; signer : Keypair.t
  ; q : unit Throttle.t
  ; mutable nonce : int option
  ; max_attempts : int
  ; delay : Time.Span.t
  }

let create ?(max_attempts = 5) ?(delay = Time.Span.of_sec 5.) ~l1_uri ~signer ()
    =
  { l1_uri
  ; signer
  ; q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; nonce = None
  ; max_attempts
  ; delay
  }

let refresh_nonce t = t.nonce <- None

let process_command t (command : Zkapp_command.t) =
  let rec retry attempt () =
    let%bind nonce =
      match t.nonce with
      | Some nonce ->
          return nonce
      | None ->
          Gql_client.fetch_nonce t.l1_uri
            (Public_key.compress t.signer.public_key)
    in
    let command =
      { command with
        fee_payer =
          { command.fee_payer with
            body =
              { command.fee_payer.body with
                nonce = Unsigned.UInt32.of_int nonce
              }
          }
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
    let signature =
      Signature_lib.Schnorr.Chunked.sign
        ~signature_kind:Mina_signature_kind.Testnet t.signer.private_key
        (Random_oracle.Input.Chunked.field full_commitment)
    in
    let command =
      { command with
        fee_payer = { command.fee_payer with authorization = signature }
      }
    in
    match%bind Gql_client.send_zkapp t.l1_uri command with
    | Ok _ ->
        t.nonce <- Option.map t.nonce ~f:(fun x -> x + 1) ;
        return ()
    | Error (`Failed_request err) when attempt >= t.max_attempts ->
        failwithf "Failed to send zkapp command: Failed_request %s" err ()
    | Error (`Graphql_error err) when attempt >= t.max_attempts ->
        failwithf "Failed to send zkapp command: Graphql_error %s" err ()
    | Error err ->
        if
          String.is_substring
            (match err with `Graphql_error s -> s | _ -> "")
            ~substring:"Account_nonce_precondition_unsatisfied"
        then refresh_nonce t ;
        retry (attempt + 1) ()
  in
  retry 0 ()

let send_zkapp_command t command =
  Throttle.enqueue t.q (fun () -> process_command t command)
