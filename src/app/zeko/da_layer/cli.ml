open Core
open Async
open Cli_lib
open Mina_base

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
       and bind_localhost =
         flag "--bind-localhost" no_arg
           ~doc:"Bind RPC and healthcheck servers to localhost only"
       and signer =
         flag "--signer" (required string)
           ~doc:"string Signer service host:port"
       and no_migrations =
         flag "--no-migrations" no_arg ~doc:"Do not run migrations"
       and network_id =
         flag "--network-id"
           (optional_with_default "zeko" string)
           ~doc:"string Network id to use as salt for applying receipts"
       and restore_from_peer =
         flag "--restore-from-peer" (optional string)
           ~doc:
             "HOST:PORT Restore an empty DA database from a trusted surviving \
              node before serving"
       and restore_target_ledger_hash =
         flag "--restore-target-ledger-hash" (optional string)
           ~doc:
             "HASH Select a peer head explicitly when the restore peer has \
              competing uncommitted branches"
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
         let restore_from_peer =
           Option.map restore_from_peer ~f:Host_and_port.of_string
         in
         let restore_target =
           Option.map restore_target_ledger_hash
             ~f:Ledger_hash.of_decimal_string
         in
         let bind_address =
           if bind_localhost then Tcp.Bind_to_address.Localhost
           else Tcp.Bind_to_address.All_addresses
         in
         let%bind signer =
           Signer_service.Client.create ~logger ~location:signer
           >>| Signer_service.Signer.of_client
         in
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ?restore_from_peer ?restore_target
                ~chain ~logger ~port ~db_dir ~healthcheck_port ~bind_address
                ~signer ~no_migrations ()
         in
         [%log info] "Server started on port %d" port ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
