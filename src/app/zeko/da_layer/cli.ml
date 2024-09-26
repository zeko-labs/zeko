open Core
open Async
open Signature_lib

let run_node =
  ( "run-node"
  , Command.async ~summary:"Run da layer node"
      (let%map_open.Command db_dir =
         flag "--db-dir"
           (optional_with_default "da_db" string)
           ~doc:"string Directory to store the database"
       and port =
         flag "--port"
           (optional_with_default 8080 int)
           ~doc:"int Port to listen on"
       and nodes_to_sync =
         flag "--da-node-to-sync" (listed string)
           ~doc:"string list Nodes to sync with"
       and testing_mode =
         flag "--random-sk" no_arg
           ~doc:"Run in testing mode, the signer key will be generated randomly"
       and sync_period =
         flag "--sync-period"
           (optional_with_default 60 int)
           ~doc:
             "int Period to sync with the nodes in seconds, 0 for no periodic \
              sync"
       in
       fun () ->
         let signer =
           if testing_mode then Private_key.(create () |> to_base58_check)
           else Sys.getenv_exn "MINA_PRIVATE_KEY"
         in
         let logger = Logger.create () in
         let nodes_to_sync =
           List.map nodes_to_sync ~f:(fun node_to_sync ->
               Cli_lib.Flag.Types.
                 { value = Core_kernel.Host_and_port.of_string node_to_sync
                 ; name = "node-to-sync"
                 } )
         in
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ~nodes_to_sync ~sync_period ~logger
                ~port ~db_dir ~signer_sk:signer ()
         in
         [%log info] "Server started on port $port"
           ~metadata:[ ("port", `Int port) ] ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
