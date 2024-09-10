open Async
open Async_kernel
open Core_kernel
open Mina_base
open Mina_transaction
open Signature_lib

let ok_exn x =
  let open Ppx_deriving_yojson_runtime.Result in
  match x with Ok x -> x | Error e -> failwith e

type t =
  { l1_uri : Uri.t Cli_lib.Flag.Types.with_name
  ; signer : Keypair.t
  ; q : unit Throttle.t
  ; mutable nonce : Account.Nonce.t option
  ; max_attempts : int
  ; delay : Time_ns.Span.t
  ; kvdb : Mina_ledger.Ledger.Kvdb.t
  }

let create ?(max_attempts = 5) ?(delay = Time_ns.Span.of_sec 5.) ?nonce ~l1_uri
    ~signer ~kvdb () =
  { l1_uri
  ; signer
  ; q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; nonce
  ; max_attempts
  ; delay
  ; kvdb
  }

let refresh_nonce t = t.nonce <- None

let increment_nonce t = t.nonce <- Option.map t.nonce ~f:Account.Nonce.(add one)

let process_command t (command : Zkapp_command.t) =
  let rec retry attempt () =
    let%bind nonce =
      match t.nonce with
      | Some nonce ->
          return nonce
      | None ->
          Gql_client.infer_nonce t.l1_uri
            (Public_key.compress t.signer.public_key)
    in
    let command =
      { command with
        fee_payer =
          { command.fee_payer with
            body = { command.fee_payer.body with nonce }
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
    let err_to_string = function
      | `Failed_request err ->
          "Failed_request: " ^ err
      | `Graphql_error err ->
          "Graphql_error: " ^ err
    in
    match%bind Gql_client.send_zkapp t.l1_uri command with
    | Ok _ ->
        printf "Sent zkapp command: %s\n%!"
          Transaction_hash.(
            to_base58_check @@ hash_command (Zkapp_command command)) ;
        return @@ increment_nonce t
    | Error err when attempt >= t.max_attempts ->
        failwithf "Failed to send zkapp command: %s" (err_to_string err) ()
    | Error err ->
        if
          String.is_substring
            (match err with `Graphql_error s -> s | _ -> "")
            ~substring:"Account_nonce_precondition_unsatisfied"
        then refresh_nonce t ;

        printf "Failed to send zkapp command: %s, retrying in %s\n%!"
          (err_to_string err)
          (Time_ns.Span.to_string t.delay) ;

        after t.delay >>= retry (attempt + 1)
  in
  retry 0 ()

let send_zkapp_command t command =
  Throttle.enqueue t.q (fun () -> process_command t command)

let wait_to_finish t = Throttle.capacity_available t.q
