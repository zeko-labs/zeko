open Async
open Core_kernel
module Ws = Websocket.Make (Cohttp_async.Io)

module Graphql_ws = struct
  type client_message =
    | Gql_connection_init
    | Gql_start of
        { id : int
        ; query : string
        ; variables : Yojson.Basic.t
        ; operation_name : string option
        }

  type server_message =
    | Gql_connection_ack
    | Gql_data of { data : Yojson.Basic.t }
    | Gql_unknown of string

  let client_message_to_string =
    Fn.compose Yojson.Basic.to_string (function
      | Gql_connection_init ->
          `Assoc [ ("type", `String "connection_init"); ("payload", `Assoc []) ]
      | Gql_start { id; query; variables; operation_name } ->
          `Assoc
            [ ("type", `String "start")
            ; ("id", `String (Int.to_string id))
            ; ( "payload"
              , `Assoc
                  [ ("query", `String query)
                  ; ("variables", variables)
                  ; ( "operationName"
                    , Option.value ~default:`Null
                      @@ Option.map ~f:(fun s -> `String s) operation_name )
                  ] )
            ] )

  let server_message_of_string s =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string s in
    let type_ = json |> member "type" |> to_string in
    match type_ with
    | "connection_ack" ->
        Gql_connection_ack
    | "data" ->
        Gql_data { data = json |> member "payload" |> member "data" }
    | _ ->
        Gql_unknown s
end

let handshake r w =
  let%bind () =
    Pipe.write w (Graphql_ws.client_message_to_string Gql_connection_init)
  in
  match%bind Pipe.read r with
  | `Ok message -> (
      match Graphql_ws.server_message_of_string message with
      | Gql_connection_ack ->
          return ()
      | _ ->
          failwith "connection_ack not received" )
  | `Eof ->
      failwith "eof"

let subscribe w =
  let subscribe_payload =
    Graphql_ws.client_message_to_string
      (Gql_start
         { id = 1
         ; query = "subscription { newTransaction }"
         ; variables = `Assoc []
         ; operation_name = None
         } )
  in
  Pipe.write w subscribe_payload

let relay_txn_to_archive_exn ~archive_uri ~logger data =
  let raw =
    Base64.decode_exn
    @@ Yojson.Basic.Util.(member "newTransaction" data |> to_string)
  in
  let diff =
    Binable.of_string (module Archive_lib.Diff.Transition_frontier) raw
  in
  match%bind
    Mina_lib.Archive_client.dispatch ~logger archive_uri
      (Archive_lib.Diff.Transition_frontier diff)
  with
  | Ok () ->
      return @@ print_endline "Relayed transaction to archive"
  | Error e ->
      failwith (Error.to_string_hum e)

let rec run ~zeko_uri ~archive_uri () =
  let () =
    match
      Thread_safe.block_on_async (fun () ->
          Conduit_async.V3.with_connection_uri zeko_uri (fun _ r w ->
              print_endline "Connected to zeko" ;
              let logger = Logger.create () in

              let r, w = Websocket_async.client_ez zeko_uri r w in

              let%bind () = handshake r w in
              let%bind () = subscribe w in

              Pipe.iter r ~f:(fun message ->
                  match Graphql_ws.server_message_of_string message with
                  | Gql_data { data } -> (
                      match%bind
                        try_with (fun () ->
                            relay_txn_to_archive_exn ~archive_uri ~logger data )
                      with
                      | Ok () ->
                          return ()
                      | Error e ->
                          return @@ print_endline (Exn.to_string e) )
                  | Gql_unknown s ->
                      return @@ print_endline ("Received unknown message: " ^ s)
                  | _ ->
                      return () ) ) )
    with
    | Ok () ->
        print_endline "Disconnected gracefully"
    | Error e ->
        print_endline (Exn.to_string e)
  in
  print_endline "Retrying in 5 seconds" ;
  Thread_safe.block_on_async_exn (fun () -> after (Time.Span.of_sec 5.)) ;
  run ~zeko_uri ~archive_uri ()

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run archive adapter for zeko"
       (let%map_open.Command zeko_uri =
          flag "--zeko-uri" (required string) ~doc:"Zeko sequencer graphql uri"
        and archive_host =
          flag "--archive-host" (required string) ~doc:"Archive node host"
        and archive_port =
          flag "--archive-port" (required int) ~doc:"Archive node port"
        in
        run ~zeko_uri:(Uri.of_string zeko_uri)
          ~archive_uri:
            { value = Host_and_port.create ~host:archive_host ~port:archive_port
            ; name = "archive-uri"
            } )
