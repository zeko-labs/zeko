(* Boots the standalone explorer backfill HTTP server and routes GraphQL query
   traffic plus GraphQL-SSE subscription traffic to the backfill modules. *)

open Core
open Async
open Cli_lib

module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let run ~logger ~port ~da_config ~nats_url () =
  let service =
    Thread_safe.block_on_async_exn (fun () ->
        Explorer_backfill_service.create ~logger ~da_config ~nats_url )
  in
  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req -> service)
      Explorer_backfill_graphql.schema
  in
  let callback ~body _sock req =
    match Uri.path (Cohttp.Request.uri req) with
    | "/graphql/stream" ->
        Explorer_backfill_sse.callback service () req body
    | _ ->
        graphql_callback () req body
  in
  let () =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            [%log error] "Unhandled exception: %s" (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      callback
    |> Deferred.ignore_m |> don't_wait_for
  in
  Shutdown.at_shutdown (fun () -> Explorer_backfill_service.shutdown service) ;
  [%log info] "Explorer backfill server listening on port %d" port ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko explorer backfill server"
    (let%map_open.Command log_json = Flag.Log.json
     and log_level = Flag.Log.level
     and port =
       flag "-p"
         (optional_with_default 8090 int)
         ~doc:
           "int Port for the standalone backfill GraphQL API; keep distinct \
            from the sequencer GraphQL port when both run on the same host"
     and da_nodes =
       flag "--da-node" (listed string)
         ~doc:"string Address of the DA node, can be supplied multiple times"
     and nats_url =
       flag "--nats-url" (required string)
         ~doc:"string NATS URL for republishing explorer events"
     in
     if List.is_empty da_nodes then failwith "--da-node must be supplied" ;
     Stdout_log.setup log_json log_level ;
     let logger = Logger.create () in
     let da_config = Da_layer.Client.Config.of_string_list da_nodes in
     let nats_url = Uri.of_string nats_url in
     run ~logger ~port ~da_config ~nats_url () )
  |> Command_unix.run
