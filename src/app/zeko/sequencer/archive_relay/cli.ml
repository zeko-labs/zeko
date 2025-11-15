open Core_kernel
open Cli_lib

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run archive adapter for zeko"
       (let%map_open.Command log_json = Flag.Log.json
        and log_level = Flag.Log.level
        and zeko_uri =
          flag "--zeko-uri" (required string) ~doc:"Zeko sequencer graphql uri"
        and da_nodes = flag "--da-node" (listed string) ~doc:"DA node uri"
        and archive_host =
          flag "--archive-host" (required string) ~doc:"Archive node host"
        and archive_port =
          flag "--archive-port" (required int) ~doc:"Archive node port"
        and sync_period =
          flag "--sync-period"
            (optional_with_default 30. float)
            ~doc:"Sync period"
        and db_dir =
          flag "--db-dir"
            (optional_with_default "archive-relay-db" string)
            ~doc:"Ledger cache"
        and network_id = flag "--network-id" (required string) ~doc:"Network id"
        and max_checkpoint_age =
          flag "--max-checkpoint-age"
            (optional_with_default 24. float)
            ~doc:"Max checkpoint age in hours"
        and checkpoint_periodicity =
          flag "--checkpoint-periodicity"
            (optional_with_default 100 int)
            ~doc:"Checkpoint periodicity in number of transactions"
        and interval_size =
          flag "--interval-size"
            (optional_with_default 100 int)
            ~doc:
              "Interval size in number of transactions, decrease in case of \
               timeouts"
        in
        let logger = Logger.create () in
        Stdout_log.setup log_json log_level ;
        let zeko_uri = Uri.of_string zeko_uri in
        let archive_uri =
          Host_and_port.create ~host:archive_host ~port:archive_port
        in
        let max_checkpoint_age = Time.Span.of_hr max_checkpoint_age in
        let chain =
          match network_id with
          | "testnet" ->
              Mina_signature_kind.Testnet
          | "mainnet" ->
              Mina_signature_kind.Mainnet
          | _ ->
              Mina_signature_kind.Other_network network_id
        in
        let t =
          Archive_relay.create ~logger ~archive_uri ~zeko_uri ~da_nodes ~db_dir
            ~chain ~max_checkpoint_age ~checkpoint_periodicity ~interval_size
        in
        Archive_relay.run t ~sync_period )
