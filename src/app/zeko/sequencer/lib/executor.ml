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
  ; signature_kind : Mina_signature_kind.t
  }

let create ?(max_attempts = 5) ?(delay = Time_ns.Span.of_sec 5.) ?nonce
    ~signature_kind ~l1_uri ~signer ~kvdb () =
  { l1_uri
  ; signature_kind
  ; signer
  ; q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; nonce
  ; max_attempts
  ; delay
  ; kvdb
  }

let refresh_nonce t = t.nonce <- None

let increment_nonce t = t.nonce <- Option.map t.nonce ~f:Account.Nonce.(add one)

let process_command ~logger t (command : Zkapp_command.t) =
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
    let command =
      Utils.sign_zkapp_command ~signature_kind:t.signature_kind command
        [ t.signer ]
    in
    let err_to_string = function
      | `Failed_request err ->
          "Failed_request: " ^ err
      | `Graphql_error err ->
          "Graphql_error: " ^ err
    in
    match%bind Gql_client.send_zkapp t.l1_uri command with
    | Ok _ ->
        [%log info] "Sent zkapp command: %s"
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

        [%log info] "Failed to send zkapp command: %s, retrying in %s"
          (err_to_string err)
          (Time_ns.Span.to_string t.delay) ;

        after t.delay >>= retry (attempt + 1)
  in
  retry 0 ()

let send_zkapp_command ~logger t command =
  Throttle.enqueue t.q (fun () -> process_command ~logger t command)

let wait_to_finish t = Throttle.capacity_available t.q
