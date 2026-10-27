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
  ; mutable settlement_reservation :
      Settlement_finality.Gateway.reservation option
  ; mutable settlement_status : string
  ; mutable settlement_error : string option
  ; mutable settlement_ready : bool
  ; settlement_owner : string
  }

let create ?(max_attempts = 5) ?(delay = Time_ns.Span.of_sec 5.) ?nonce
    ?settlement_owner ~signature_kind ~signer ~kind () =
  { kind
  ; signature_kind
  ; signer
  ; q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; nonce
  ; max_attempts
  ; delay
  ; settlement_reservation = None
  ; settlement_status =
      ( if Settlement_finality.ethereum_gateway_enabled () then "initializing"
      else "ready" )
  ; settlement_error = None
  ; settlement_ready = not (Settlement_finality.ethereum_gateway_enabled ())
  ; settlement_owner =
      Option.value settlement_owner
        ~default:
          (Public_key.Compressed.to_base58_check
             (Signer_service.Signer.public_key signer) )
  }

let settlement_status t ~ready status error =
  t.settlement_ready <-
    ready || not (Settlement_finality.ethereum_gateway_enabled ()) ;
  t.settlement_status <- status ;
  t.settlement_error <- error

let pause_settlement t error =
  settlement_status t ~ready:false
    ( if String.equal t.settlement_status "blocked" then "blocked"
    else "recovering" )
    (Some (Error.to_string_hum error))

let release_reservation t =
  match (t.kind, t.settlement_reservation) with
  | `L1 l1_uri, Some reservation ->
      t.settlement_reservation <- None ;
      Settlement_finality.Gateway.release ~l1_uri reservation
  | _ ->
      Deferred.unit

let rec reserve_settlement t =
  let%bind result =
    match t.kind with
    | `L1 l1_uri when Settlement_finality.ethereum_gateway_enabled () -> (
        let open Deferred.Or_error.Let_syntax in
        match t.settlement_reservation with
        | Some reservation when not reservation.stopped ->
            Settlement_finality.Gateway.renew ~l1_uri reservation
        | _ ->
            let%map reservation =
              Settlement_finality.Gateway.acquire ~l1_uri
                ~owner_id:("zeko-sequencer:" ^ t.settlement_owner)
            in
            t.settlement_reservation <- Some reservation )
    | _ ->
        Deferred.Or_error.return ()
  in
  match result with
  | Error error
    when String.is_substring
           (Error.to_string_hum error)
           ~substring:"OUTER_WRITER_BUSY" ->
      settlement_status t ~ready:t.settlement_ready "waiting_for_outer_writer"
        None ;
      let%bind () = Clock_ns.after (Time_ns.Span.of_sec 15.) in
      reserve_settlement t
  | _ ->
      Deferred.return result

let refresh_nonce t = t.nonce <- None

let increment_nonce t = t.nonce <- Option.map t.nonce ~f:Account.Nonce.(add one)

let infer_nonce ~logger t pk =
  match t.kind with
  | `L1 l1_uri ->
      Gql_client.infer_nonce ~logger l1_uri pk
  | `L2 { infer_nonce; _ } ->
      return (Ok (infer_nonce pk))

let process_command ~logger ?settlement_export ?before_send t
    (command : Zkapp_command.t) =
  (* A transport retry is the same submission, including its signature and
     reservation. Re-signing can change the gateway idempotency key. *)
  let prepared_command = ref None in
  let prepared_payload = ref None in
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
      let%bind.Deferred.Result command =
        match !prepared_command with
        | Some command ->
            return (Ok command)
        | None ->
            let%bind.Deferred.Result nonce =
              match t.nonce with
              | Some nonce ->
                  return (Ok nonce)
              | None ->
                  infer_nonce ~logger t
                    (Signer_service.Signer.public_key t.signer)
                  >>| Result.map_error ~f:(fun err ->
                          `Nonce_inference_error err )
            in
            let command =
              { command with
                fee_payer =
                  { command.fee_payer with
                    body = { command.fee_payer.body with nonce }
                  }
              }
            in
            let%map.Deferred.Result command =
              Signer_service.Signer.sign_zkapp_command
                ~signature_kind:t.signature_kind t.signer
                (Zkapp_command.read_all_proofs_from_disk command)
              >>| Result.map_error ~f:(fun err ->
                      `Send_zkapp_error
                        (`Failed_request (Error.to_string_hum err)) )
            in
            prepared_command := Some command ;
            command
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
            let settlement =
              if attach_settlement then
                Option.map settlement_export ~f:(fun export ->
                    match !prepared_payload with
                    | Some payload ->
                        payload
                    | None ->
                        let json =
                          Ethereum_settlement_export.to_gateway_json export
                            command
                        in
                        let payload =
                          match (json, t.settlement_reservation) with
                          | `Assoc fields, Some reservation ->
                              `Assoc
                                ( ( "reservation"
                                  , Settlement_finality.Gateway.reservation_json
                                      reservation )
                                :: fields )
                          | _ ->
                              json
                        in
                        prepared_payload := Some payload ;
                        payload )
              else None
            in
            let%bind.Deferred.Result () =
              match (settlement, before_send) with
              | Some payload, Some persist ->
                  persist command payload
                  >>| Result.map_error ~f:(fun e ->
                          `Send_zkapp_error
                            (`Failed_request (Error.to_string_hum e)) )
              | _ ->
                  return (Ok ())
            in
            let%bind.Deferred.Result () =
              match (settlement, t.settlement_reservation) with
              | Some _, Some reservation ->
                  Settlement_finality.Gateway.renew ~l1_uri reservation
                  >>| Result.map_error ~f:(fun e ->
                          `Send_zkapp_error
                            (`Failed_request (Error.to_string_hum e)) )
              | _ ->
                  return (Ok ())
            in
            Gql_client.send_zkapp
              ?settlement:(Option.map settlement ~f:Yojson.Safe.to_basic)
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
        [%log info] "Sent zkapp command: %s"
          Transaction_hash.(
            to_base58_check @@ hash_command (Zkapp_command command)) ;
        increment_nonce t ;
        let hash : Mina_transaction.Transaction_hash.t =
          Mina_transaction.Transaction_hash.hash_command (Zkapp_command command)
        in
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
        then (
          refresh_nonce t ;
          prepared_command := None ;
          prepared_payload := None ) ;

        [%log info] "Failed to send zkapp command: %s, retrying in %s"
          (err_to_string err)
          (Time_ns.Span.to_string t.delay) ;

        after t.delay >>= retry (attempt + 1)
  in
  retry 0 ()

let send_zkapp_command ~logger ?settlement_export ?before_send t command =
  Throttle.enqueue t.q (fun () ->
      Monitor.try_with ~here:[%here] (fun () ->
          process_command ~logger ?settlement_export ?before_send t command )
      >>| Result.map_error ~f:Error.of_exn
      >>| Or_error.join )

let wait_to_finish t = Throttle.capacity_available t.q

let replay_settlement t ~l1_uri ~command_base64 ~payload =
  Throttle.enqueue t.q (fun () ->
      Monitor.try_with (fun () ->
          match Zkapp_command.of_base64 command_base64 with
          | Error error ->
              Deferred.Or_error.errorf
                "Invalid persisted settlement command: %s"
                (Error.to_string_hum error)
          | Ok command -> (
              let%map result =
                Gql_client.send_zkapp
                  ~settlement:(Yojson.Safe.to_basic payload)
                  l1_uri command
              in
              match result with
              | Ok _ ->
                  refresh_nonce t ; Ok ()
              | Error (`Failed_request error | `Graphql_error error) ->
                  Or_error.error_string error ) )
      >>| Result.map_error ~f:Error.of_exn
      >>| Or_error.join )
