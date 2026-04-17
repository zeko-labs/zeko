(* Defines the shared explorer-facing NATS subjects, payload encoding, and
   publishing helpers used by both the live sequencer path and backfill jobs. *)

open Core_kernel
open Async
open Mina_base

module Transaction_kind = struct
  type t = User_command | Fee_transfer | Sync_replay | Genesis_replay

  let to_string = function
    | User_command ->
        "user_command"
    | Fee_transfer ->
        "fee_transfer"
    | Sync_replay ->
        "sync_replay"
    | Genesis_replay ->
        "genesis_replay"
end

module Finality_status = struct
  type t = Proved | Committed

  let to_string = function Proved -> "proved" | Committed -> "committed"
end

module Subject = struct
  let transactions = "zeko.l2.transactions"

  let finality = "zeko.l2.finality"

  let health = "zeko.health"
end

module Jetstream = struct
  let stream_name = "ZEKO_L2"

  let duplicate_window_ns = 120_000_000_000

  let subjects = [ Subject.transactions; Subject.finality; Subject.health ]

  let stream_config =
    `Assoc
      [ ("name", `String stream_name)
      ; ( "subjects"
        , `List (List.map subjects ~f:(fun subject -> `String subject)) )
      ; ("retention", `String "limits")
      ; ("storage", `String "file")
      ; ("discard", `String "old")
      ; ("max_msgs", `Int (-1))
      ; ("max_bytes", `Int (-1))
      ; ("max_age", `Int 0)
      ; ("max_msgs_per_subject", `Int (-1))
      ; ("max_msg_size", `Int (-1))
      ; ("num_replicas", `Int 1)
      ; ("duplicate_window", `Int duplicate_window_ns)
      ]

  let api_subject operation =
    sprintf "$JS.API.STREAM.%s.%s" operation stream_name
end

type message =
  { subject : string
  ; headers : (string * string) list
  ; payload : Yojson.Safe.t
  }

type sink = message -> unit

let noop_sink _ = ()

let timestamp_json ~logger =
  Block_time.now (Block_time.Controller.basic ~logger) |> Block_time.to_yojson

let nats_msg_id target_ledger_hash =
  Ledger_hash.to_decimal_string target_ledger_hash

let nats_msg_id_headers target_ledger_hash =
  [ ("Nats-Msg-Id", nats_msg_id target_ledger_hash) ]

let finality_nats_msg_id ~status target_ledger_hash =
  sprintf "finality-%s-%s"
    (Finality_status.to_string status)
    (Ledger_hash.to_decimal_string target_ledger_hash)

let finality_nats_msg_id_headers ~status target_ledger_hash =
  [ ("Nats-Msg-Id", finality_nats_msg_id ~status target_ledger_hash) ]

let create_nats_sink ?logger client : sink =
 fun { subject; headers; payload } ->
  let headers = Nats_client.Headers.of_list headers in
  match
    Nats_client_async.publish_result client ~subject ~headers
      (Yojson.Safe.to_string payload)
  with
  | `Queued ->
      ()
  | `Dropped ->
      Option.iter logger ~f:(fun logger ->
          [%log warn] "Dropped explorer NATS message"
            ~metadata:[ ("subject", `String subject) ] )

let jetstream_request_timeout = Time_ns.Span.of_sec 5.

let warn_jetstream ~logger message ~metadata =
  [%log warn] "%s" message
    ~metadata:(("stream", `String Jetstream.stream_name) :: metadata)

let response_error json =
  match json with
  | `Assoc fields -> (
      match List.Assoc.find fields "error" ~equal:String.equal with
      | Some (`Assoc error_fields) ->
          Some error_fields
      | _ ->
          None )
  | _ ->
      None

let response_error_int fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some (`Int value) ->
      Some value
  | _ ->
      None

let response_error_string fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some (`String value) ->
      Some value
  | _ ->
      None

let is_stream_not_found fields =
  Option.value_map
    (response_error_int fields "err_code")
    ~default:false ~f:(Int.equal 10059)
  || Option.value_map
       (response_error_int fields "code")
       ~default:false ~f:(Int.equal 404)

let jetstream_request client ~operation payload =
  let subject = Jetstream.api_subject operation in
  Nats_client_async.request client ~subject ~timeout:jetstream_request_timeout
    (Yojson.Safe.to_string payload)

let jetstream_response_result ~operation response =
  match Or_error.try_with (fun () -> Yojson.Safe.from_string response) with
  | Error error ->
      Error (Error.tag error ~tag:"Could not parse JetStream response")
  | Ok json -> (
      match response_error json with
      | None ->
          Ok ()
      | Some fields ->
          let description =
            Option.value
              (response_error_string fields "description")
              ~default:"unknown JetStream error"
          in
          Or_error.errorf "JetStream stream %s failed: %s" operation
            description )

let create_or_update_jetstream_stream_result client ~operation =
  jetstream_request client ~operation Jetstream.stream_config
  >>| function
  | Error error ->
      Error
        (Error.tag error
           ~tag:(sprintf "JetStream stream %s request failed" operation) )
  | Ok response ->
      jetstream_response_result ~operation response

let ensure_jetstream_stream_result client =
  let open Deferred.Let_syntax in
  let%bind info = jetstream_request client ~operation:"INFO" (`Assoc []) in
  match info with
  | Error error ->
      return (Error (Error.tag error ~tag:"JetStream stream INFO request failed"))
  | Ok response -> (
      match Or_error.try_with (fun () -> Yojson.Safe.from_string response) with
      | Error error ->
          return
            (Error
               (Error.tag error
                  ~tag:"Could not parse JetStream stream INFO response" ) )
      | Ok json -> (
          match response_error json with
          | Some fields when is_stream_not_found fields ->
              create_or_update_jetstream_stream_result client
                ~operation:"CREATE"
          | Some fields ->
              let description =
                Option.value
                  (response_error_string fields "description")
                  ~default:"unknown JetStream error"
              in
              Deferred.return
                (Or_error.errorf "JetStream stream INFO failed: %s"
                   description )
          | None ->
              create_or_update_jetstream_stream_result client
                ~operation:"UPDATE" ) )

let ensure_jetstream_stream ~logger client =
  ensure_jetstream_stream_result client >>| function
  | Ok () ->
      ()
  | Error error ->
      warn_jetstream ~logger "JetStream stream setup failed"
        ~metadata:[ ("error", `String (Error.to_string_hum error)) ]

let nats_uri_available ?(timeout = Time_ns.Span.of_sec 3.) ~logger uri =
  match Uri.host uri with
  | None ->
      warn_jetstream ~logger "NATS URL is missing a host"
        ~metadata:[ ("uri", `String (Uri.to_string uri)) ] ;
      Deferred.return false
  | Some host -> (
      let port = Uri.port uri |> Option.value ~default:4222 in
      let where =
        Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port)
      in
      Clock_ns.with_timeout timeout
        (Monitor.try_with_or_error (fun () ->
             let%bind _socket, reader, writer = Tcp.connect where in
             let%bind () = Writer.close writer in
             Reader.close reader ) )
      >>| function
      | `Timeout ->
          warn_jetstream ~logger
            "Timed out checking NATS availability; explorer publishing disabled"
            ~metadata:[ ("uri", `String (Uri.to_string uri)) ] ;
          false
      | `Result (Error error) ->
          warn_jetstream ~logger
            "NATS is unavailable; explorer publishing disabled"
            ~metadata:
              [ ("uri", `String (Uri.to_string uri))
              ; ("error", `String (Error.to_string_hum error))
              ] ;
          false
      | `Result (Ok ()) ->
          true )

let connect_and_ensure_jetstream ?(timeout = Time_ns.Span.of_sec 5.) ~logger uri
    =
  let open Deferred.Let_syntax in
  let%bind available = nats_uri_available ~logger uri in
  if not available then return None
  else
    let connect =
      Monitor.try_with_or_error (fun () -> Nats_client_async.connect (Some uri))
    in
    let%bind connected =
      Clock_ns.with_timeout timeout connect
    in
    match connected with
    | `Timeout ->
        Deferred.upon connect (function
          | Ok client ->
              don't_wait_for (Nats_client_async.close client)
          | Error _ ->
              () ) ;
        warn_jetstream ~logger
          "Timed out connecting to NATS; explorer publishing disabled"
          ~metadata:[ ("uri", `String (Uri.to_string uri)) ] ;
        return None
    | `Result (Error error) ->
        warn_jetstream ~logger
          "Failed to connect to NATS; explorer publishing disabled"
          ~metadata:
            [ ("uri", `String (Uri.to_string uri))
            ; ("error", `String (Error.to_string_hum error))
            ] ;
        return None
    | `Result (Ok client) ->
        let%map ensure_result =
          Monitor.try_with_or_error (fun () ->
              ensure_jetstream_stream ~logger client )
        in
        Result.iter_error ensure_result ~f:(fun error ->
            warn_jetstream ~logger "JetStream setup raised an exception"
              ~metadata:[ ("error", `String (Error.to_string_hum error)) ] ) ;
        Some client

let connect_and_ensure_jetstream_strict ?timeout ~logger uri =
  let open Deferred.Let_syntax in
  let%bind client = connect_and_ensure_jetstream ?timeout ~logger uri in
  match client with
  | None ->
      Deferred.return
        (Or_error.errorf "Could not connect to NATS at %s" (Uri.to_string uri))
  | Some client ->
      let%map result = ensure_jetstream_stream_result client in
      Result.map_error result ~f:(fun error ->
          don't_wait_for (Nats_client_async.close client) ;
          Error.tag error ~tag:"Could not configure JetStream" )
      |> Result.map ~f:(fun () -> client)

let build_transaction_message ~kind ~target_ledger_hash ~genesis ~diff =
  { subject = Subject.transactions
  ; headers = nats_msg_id_headers target_ledger_hash
  ; payload =
      `Assoc
        [ ("kind", `String (Transaction_kind.to_string kind))
        ; ("target_ledger_hash", Ledger_hash.to_yojson target_ledger_hash)
        ; ("genesis", `Bool genesis)
        ; ("diff", Da_layer.Diff.Stable.V3.to_yojson diff)
        ]
  }

let build_live_diff ~logger ~diff ~acc_set_root =
  Da_layer.Diff.add_time_and_acc_set ~logger diff ~acc_set:acc_set_root

let build_finality_message ~logger ~status ~source_ledger_hash
    ~target_ledger_hash =
  { subject = Subject.finality
  ; headers = finality_nats_msg_id_headers ~status target_ledger_hash
  ; payload =
      `Assoc
        [ ("status", `String (Finality_status.to_string status))
        ; ("source_ledger_hash", Ledger_hash.to_yojson source_ledger_hash)
        ; ("target_ledger_hash", Ledger_hash.to_yojson target_ledger_hash)
        ; ("timestamp", timestamp_json ~logger)
        ]
  }

let build_health_message ~logger ~service ~instance_id ~status
    ?last_published_hash ?unproved_hash () =
  let optional_hash name = function
    | None ->
        []
    | Some hash ->
        [ (name, Ledger_hash.to_yojson hash) ]
  in
  { subject = Subject.health
  ; headers = []
  ; payload =
      `Assoc
        ( [ ("service", `String service)
          ; ("instance_id", `String instance_id)
          ; ("status", `String status)
          ]
        @ optional_hash "last_published_hash" last_published_hash
        @ optional_hash "unproved_hash" unproved_hash
        @ [ ("timestamp", timestamp_json ~logger) ] )
  }

let publish sink message = sink message

let publish_transaction sink ~kind ~target_ledger_hash ~genesis ~diff =
  publish sink
  @@ build_transaction_message ~kind ~target_ledger_hash ~genesis ~diff

let publish_finality sink ~logger ~status ~source_ledger_hash
    ~target_ledger_hash =
  publish sink
  @@ build_finality_message ~logger ~status ~source_ledger_hash
       ~target_ledger_hash

let publish_health sink ~logger ~service ~instance_id ~status
    ?last_published_hash ?unproved_hash () =
  publish sink
  @@ build_health_message ~logger ~service ~instance_id ~status
       ?last_published_hash ?unproved_hash ()
