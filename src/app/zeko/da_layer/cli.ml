open Core
open Async
open Signature_lib
open Mina_base
open Mina_ledger
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
       in
       fun () ->
         let signer =
           if testing_mode then Private_key.(create () |> to_base58_check)
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
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ~sync_arg ~logger ~port ~db_dir
                ~signer_sk:signer ~no_migrations ()
         in
         [%log info] "Server started on port $port"
           ~metadata:[ ("port", `Int port) ] ;
         Async.never () ) )

let sync_node =
  ( "sync-node"
  , Command.async ~summary:"Sync node"
      (let%map_open.Command synced_node =
         flag "--synced-node" (required string)
           ~doc:"string Node that is providing data"
       and unsynced_node =
         flag "--unsynced-node" (required string)
           ~doc:"string Node that needs to be synced"
       and hash_to_sync =
         flag "--hash-to-sync" (required string)
           ~doc:"string Hash to sync with in decimal string form"
       in
       fun () ->
         let logger = Logger.create () in
         [%log info] "Fetching intervals" ;
         let ledger =
           Ledger.create_ephemeral
             ~depth:Da_layer.Node.constraint_constants.ledger_depth ()
         in
         let synced_node =
           Cli_lib.Flag.Types.
             { value = Host_and_port.of_string synced_node
             ; name = sprintf "synced_node"
             }
         in
         let unsynced_node =
           Cli_lib.Flag.Types.
             { value = Host_and_port.of_string unsynced_node
             ; name = sprintf "unsynced_node"
             }
         in
         Da_layer.Client.map_diffs ~logger
           ~depth:Da_layer.Node.constraint_constants.ledger_depth
           ~config:(Da_layer.Client.Config.of_node_locations [ synced_node ])
           ~source_ledger_hash:`Genesis
           ~target_ledger_hash:(Ledger_hash.of_decimal_string hash_to_sync)
           ~f:(fun ~current_chunk ~chunks_length diff ->
             let progress =
               Float.of_int current_chunk /. Float.of_int chunks_length
             in
             Zeko_util.progress_bar progress ;
             let diff = Da_layer.Diff.drop_time diff in
             let ledger_openings = Da_layer.Client.get_openings ~diff ~ledger in
             match%bind
               Da_layer.Client.Rpc.post_diff ~logger
                 ~node_location:unsynced_node ~diff ~ledger_openings
             with
             | Ok _signature ->
                 return ()
             | Error e ->
                 [%log warn] "Error posting diff: $error"
                   ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
                 Error.raise e )
         >>| Or_error.ok_exn >>| ignore ) )

let () =
  Command.group ~summary:"DA layer CLI" [ run_node; sync_node ]
  |> Command_unix.run
