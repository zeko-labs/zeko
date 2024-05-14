open Async
open Async_kernel
open Core_kernel
open Mina_base
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
  ; delay : Time.Span.t
  ; kvdb : Kvdb.t
  }

let create ?(max_attempts = 5) ?(delay = Time.Span.of_sec 5.) ?nonce ~l1_uri
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
          Gql_client.inferr_nonce t.l1_uri
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
    match%bind Gql_client.send_zkapp t.l1_uri command with
    | Ok _ ->
        return @@ increment_nonce t
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

module Commits_store = struct
  type commit_id = Frozen_ledger_hash.t * Frozen_ledger_hash.t
  [@@deriving yojson]

  type index = commit_id list [@@deriving yojson]

  let store_commit kvdb command ~source ~target =
    (* Update index *)
    let index =
      Kvdb.get kvdb ~key:COMMIT_INDEX
      |> Option.map ~f:(fun data ->
             ok_exn @@ index_of_yojson @@ Yojson.Safe.from_string
             @@ Bigstring.to_string data )
      |> Option.value ~default:[]
    in
    let index = (source, target) :: index in
    Kvdb.set kvdb ~key:COMMIT_INDEX
      ~data:
        (Bigstring.of_string @@ Yojson.Safe.to_string @@ index_to_yojson index) ;

    (* Store commit *)
    let commit_id = (source, target) in
    Kvdb.set kvdb ~key:(COMMIT commit_id)
      ~data:
        ( Bigstring.of_string @@ Yojson.Safe.to_string
        @@ Zkapp_command.to_yojson command )

  let load_commit_exn kvdb commit_id =
    let data = Option.value_exn @@ Kvdb.get kvdb ~key:(COMMIT commit_id) in
    ok_exn @@ Zkapp_command.of_yojson @@ Yojson.Safe.from_string
    @@ Bigstring.to_string data

  let get_index kvdb =
    Kvdb.get kvdb ~key:COMMIT_INDEX
    |> Option.map ~f:(fun data ->
           ok_exn @@ index_of_yojson @@ Yojson.Safe.from_string
           @@ Bigstring.to_string data )
    |> Option.value ~default:[]

  let get_commit kvdb ~source ~target =
    let index = get_index kvdb in
    let%bind.Option commit_id =
      List.find index ~f:(fun (s, t) ->
          Frozen_ledger_hash.equal s source && Frozen_ledger_hash.equal t target )
    in
    Some (load_commit_exn kvdb commit_id)

  let get_all kvdb =
    let index = get_index kvdb in
    let commits = List.map index ~f:(load_commit_exn kvdb) in
    return commits
end

let send_commit t command ~source ~target =
  Commits_store.store_commit t.kvdb command ~source ~target ;
  send_zkapp_command t command

let wait_to_finish t = Throttle.capacity_available t.q
