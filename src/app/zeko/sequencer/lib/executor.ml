open Async_kernel
open Core_kernel
open Mina_base
open Mina_transaction
open Signature_lib

module Sequencer_harness = struct
  type t =
    { infer_nonce : Public_key.Compressed.t -> Account.Nonce.t
    ; apply_user_command : User_command.t -> (unit, Error.t) result Deferred.t
    }
end

type t =
  { kind : [ `L1 of Uri.t | `L2 of Sequencer_harness.t ]
  ; signer : Signer_service.Signer.t
  ; q : unit Throttle.t
  ; mutable nonce : Account.Nonce.t option
  ; max_attempts : int
  ; delay : Time_ns.Span.t
  ; signature_kind : Mina_signature_kind.t
  }

let create ?(max_attempts = 5) ?(delay = Time_ns.Span.of_sec 5.) ?nonce
    ~signature_kind ~signer ~kind () =
  { kind
  ; signature_kind
  ; signer
  ; q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; nonce
  ; max_attempts
  ; delay
  }

let refresh_nonce t = t.nonce <- None

let increment_nonce t = t.nonce <- Option.map t.nonce ~f:Account.Nonce.(add one)

let infer_nonce ~logger t pk =
  match t.kind with
  | `L1 l1_uri ->
      Gql_client.infer_nonce ~logger l1_uri pk
  | `L2 { infer_nonce; _ } ->
      return (Ok (infer_nonce pk))

let process_command ~logger ?settlement_export t (command : Zkapp_command.t) =
  let rec retry attempt () =
    let err_to_string = function
      | `Nonce_inference_error err ->
          "Nonce_inference_error: " ^ Error.to_string_hum err
      | `Send_zkapp_error (`Failed_request err) ->
          "Failed_request: " ^ err
      | `Send_zkapp_error (`Graphql_error err) ->
          "Graphql_error: " ^ err
    in
    match%bind
      let%bind.Deferred.Result nonce =
        match t.nonce with
        | Some nonce ->
            return (Ok nonce)
        | None ->
            infer_nonce ~logger t (Signer_service.Signer.public_key t.signer)
            >>| Result.map_error ~f:(fun err -> `Nonce_inference_error err)
      in
      let command =
        { command with
          fee_payer =
            { command.fee_payer with
              body = { command.fee_payer.body with nonce }
            }
        }
      in
      let%bind.Deferred.Result command =
        Signer_service.Signer.sign_zkapp_command
          ~signature_kind:t.signature_kind t.signer
          (Zkapp_command.read_all_proofs_from_disk command)
        >>| Result.map_error ~f:(fun err ->
                `Send_zkapp_error (`Failed_request (Error.to_string_hum err)) )
      in
      let%map.Deferred.Result () =
        Option.iter settlement_export ~f:(fun export ->
            Ethereum_settlement_export.maybe_write_gateway_fixture export
              command ) ;
        let attach_settlement =
          not
            (Option.value_map
               (Sys.getenv_opt "ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_ONLY")
               ~default:false
               ~f:(String.Caseless.equal "true") )
        in
        match t.kind with
        | `L1 l1_uri ->
            Gql_client.send_zkapp
              ?settlement:
                ( if attach_settlement then
                  Option.map settlement_export ~f:(fun export ->
                      Ethereum_settlement_export.to_gateway_json export command
                      |> Yojson.Safe.to_basic )
                else None )
              l1_uri command
            >>| Result.map_error ~f:(fun err -> `Send_zkapp_error err)
            >>| Result.map ~f:ignore
        | `L2 { apply_user_command; _ } ->
            apply_user_command
              (Zkapp_command
                 (Zkapp_command.write_all_proofs_to_disk
                    ~signature_kind:t.signature_kind
                    ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
                    command ) )
            >>| Result.map_error ~f:(fun e ->
                    `Send_zkapp_error (`Failed_request (Error.to_string_hum e)) )
      in
      command
    with
    | Ok command ->
        let hash =
          Mina_transaction.Transaction_hash.hash_command (Zkapp_command command)
        in
        [%log info] "Sent zkapp command: %s"
          (Mina_transaction.Transaction_hash.to_base58_check hash) ;
        increment_nonce t ;
        return (Ok hash)
    | Error err when attempt >= t.max_attempts ->
        return
          (Error
             (Error.of_string
                (sprintf "Failed to send zkapp command: %s" (err_to_string err)) )
          )
    | Error err ->
        if
          String.is_substring
            ( match err with
            | `Send_zkapp_error (`Graphql_error s) ->
                s
            | _ ->
                "" )
            ~substring:"Account_nonce_precondition_unsatisfied"
        then refresh_nonce t ;

        [%log info] "Failed to send zkapp command: %s, retrying in %s"
          (err_to_string err)
          (Time_ns.Span.to_string t.delay) ;

        after t.delay >>= retry (attempt + 1)
  in
  retry 0 ()

let send_zkapp_command ~logger ?settlement_export t command =
  Throttle.enqueue t.q (fun () ->
      Monitor.try_with ~here:[%here] (fun () ->
          process_command ~logger ?settlement_export t command )
      >>| Result.map_error ~f:Error.of_exn
      >>| Or_error.join )

let wait_to_finish t = Throttle.capacity_available t.q
