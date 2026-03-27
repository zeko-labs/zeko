open Core
open Async
open Signature_lib
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
       and testing_mode =
         flag "--random-sk" no_arg
           ~doc:"Run in testing mode, the signer key will be generated randomly"
       and no_migrations =
         flag "--no-migrations" no_arg ~doc:"Do not run migrations"
       and network_id =
         flag "--network-id"
           (optional_with_default "zeko" string)
           ~doc:"string Network id to use as salt for applying receipts"
       in
       fun () ->
         let signer =
           if testing_mode then
             let rec create_even_signer () =
               let signer = Keypair.create () in
               let compressed = Public_key.compress signer.public_key in
               if compressed.is_odd then create_even_signer () else signer
             in
             (create_even_signer ()).private_key |> Private_key.to_base58_check
           else Sys.getenv_exn "MINA_PRIVATE_KEY"
         in
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
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ~chain ~logger ~port ~db_dir
                ~healthcheck_port ~signer_sk:signer ~no_migrations ()
         in
         [%log info] "Server started on port %d" port ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
