open Core
open Async
open Cli_lib

let run_node =
  ( "run-node"
  , Command.async ~summary:"Run da layer node"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and db_dir =
         flag "--db-dir"
           (optional_with_default "da_db" string)
           ~doc:"string Directory to store the database"
       and port =
         flag "--port"
           (optional_with_default 8080 int)
           ~doc:"int Port to listen on"
       and healthcheck_port =
         flag "--healthcheck-port"
           (optional_with_default 8081 int)
           ~doc:"int Optional HTTP port exposing /health for simple probes"
       and signer =
         flag "--signer" (required string)
           ~doc:"string Signer service host:port"
       and no_migrations =
         flag "--no-migrations" no_arg ~doc:"Do not run migrations"
       and network_id =
         flag "--network-id"
           (optional_with_default "zeko" string)
           ~doc:"string Network id to use as salt for applying receipts"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let chain =
           match network_id with
           | "mainnet" ->
               Mina_signature_kind.Mainnet
           | "testnet" ->
               Mina_signature_kind.Testnet
           | network_id ->
               Mina_signature_kind.Other_network network_id
         in
         let signer = Host_and_port.of_string signer in
         let%bind signer =
           Signer_service.Client.create ~logger ~location:signer
           >>| Signer_service.Signer.of_client
         in
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ~chain ~logger ~port ~db_dir
                ~healthcheck_port ~signer ~no_migrations ()
         in
         [%log info] "Server started on port %d" port ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
