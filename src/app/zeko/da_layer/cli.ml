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
       and node_to_sync =
         flag "--da-node-to-sync" (optional string)
           ~doc:"string Nodes to sync with"
       and hash_to_sync =
         flag "--hash-to-sync" (optional string)
           ~doc:"string Hash to sync with in decimal string form"
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
         let sync_arg =
           match (node_to_sync, hash_to_sync) with
           | Some node_to_sync, Some hash_to_sync ->
               Some
                 ( Cli_lib.Flag.Types.
                     { value = Core_kernel.Host_and_port.of_string node_to_sync
                     ; name = "node-to-sync"
                     }
                 , Mina_base.Ledger_hash.of_decimal_string hash_to_sync )
           | None, None ->
               None
           | _ ->
               failwith "Both node-to-sync and hash-to-sync must be provided"
         in
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
           @@ Da_layer.Node.create_server ~chain ~sync_arg ~logger ~port ~db_dir
                ~signer_sk:signer ~no_migrations ()
         in
         [%log info] "Server started on port %d" port ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
