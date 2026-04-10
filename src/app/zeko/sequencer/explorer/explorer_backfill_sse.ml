(* Implements the GraphQL-SSE transport for the standalone backfill server. *)

open Core
open Async

let headers =
  Cohttp.Header.of_list
    [ ("Content-Type", "text/event-stream")
    ; ("Cache-Control", "no-cache")
    ; ("Connection", "keep-alive")
    ]

let next_event payload =
  "event: next\ndata: " ^ Yojson.Basic.to_string payload ^ "\n\n"

let complete_event = "event: complete\ndata: {}\n\n"

let parse_request req body =
  Init.Graphql_internal.Params.extract req body
  |> Result.map_error ~f:Error.of_string

let execute_subscription t req body =
  let open Deferred.Let_syntax in
  match parse_request req body with
  | Error err ->
      Deferred.return (Error err)
  | Ok (query, variables, operation_name) -> (
      match Graphql_parser.parse query with
      | Error err ->
          Deferred.return (Error (Error.of_string err))
      | Ok doc ->
          let%map result =
            Graphql_async.Schema.execute Explorer_backfill_graphql.schema t
              ?variables ?operation_name doc
          in
          match result with
          | Ok (`Stream stream) ->
              Ok stream
          | Ok (`Response _) ->
              Error
                (Error.of_string
                   "Expected a GraphQL subscription for /graphql/stream")
          | Error err ->
              Error
                (Error.of_string
                   ("Invalid GraphQL subscription: "
                   ^ Yojson.Basic.to_string err ) ) )

let rec write_stream body_writer stream =
  let open Deferred.Let_syntax in
  match%bind Pipe.read stream with
  | `Eof ->
      let%map () = Pipe.write body_writer complete_event in
      Pipe.close body_writer
  | `Ok payload ->
      let payload =
        match payload with
        | Ok payload ->
            payload
        | Error err ->
            err
      in
      let%bind () = Pipe.write body_writer (next_event payload) in
      write_stream body_writer stream

let callback t _conn req body =
  let open Deferred.Let_syntax in
  let%bind body = Cohttp_async.Body.to_string body in
  match%bind execute_subscription t req body with
  | Error err ->
      Cohttp_async.Server.respond_string ~status:`Bad_request
        (Error.to_string_hum err)
      >>| fun response -> `Response response
  | Ok stream ->
      let body_reader, body_writer = Pipe.create () in
      don't_wait_for (write_stream body_writer stream) ;
      Cohttp_async.Server.respond_with_pipe ~headers body_reader
      >>| fun response -> `Response response
