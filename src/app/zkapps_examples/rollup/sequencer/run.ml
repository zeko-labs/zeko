open Core
open Async
open Sequencer_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let run port max_pool_size commitment_period da_contract_address db_dir () =
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Zeko_sequencer.bootstrap ~max_pool_size
          ~committment_period_sec:commitment_period ~da_contract_address ~db_dir )
  in

  Zeko_sequencer.run_committer sequencer ;

  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req -> sequencer)
      Gql.schema
  in
  let () =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            print_endline "Unhandled exception" ;
            print_endline (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _sock req ->
        let headers = Cohttp.Request.headers req in
        match Cohttp.Header.get headers "Connection" with
        | Some "Upgrade" ->
            Graphql_cohttp_async.respond_string ~status:`Forbidden
              ~body:"Websocket not supported" ()
        | _ ->
            graphql_callback () req body )
    |> Deferred.ignore_m |> don't_wait_for
  in
  print_endline ("Sequencer listening on port " ^ Int.to_string port) ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko sequencer"
    (let%map_open.Command port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and commitment_period =
       flag "--committment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 10 int)
         ~doc:"int Maximum transaction pool size"
     and da_contract_address =
       flag "--da-contract-address" (optional string)
         ~doc:"string Address of the DA contract"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "db" string)
         ~doc:"string Directory to store the database"
     in
     run port max_pool_size commitment_period da_contract_address db_dir )
  |> Command_unix.run
