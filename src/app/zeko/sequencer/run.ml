(* Boots the main Zeko sequencer HTTP service and threads runtime CLI options,
   including optional explorer NATS publishing, into the sequencer runtime. *)

open Core
open Async
open Sequencer_lib
open Cli_lib
open Signature_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)
module Sequencer = Zeko_sequencer.Sequencer

let run ~logger ~port ~max_pool_size ~commitment_period ~da_config ~da_keys
    ~da_quorum ~db_dir ~checkpoints_dir ~postgres_uri ~l1_uri ~archive_uri
    ~signer ~deposit_delay_blocks ~mq_host ~fee_modifier ~minimum_fee ~nats_url
    ~slot_acceptance ~commit_validity_period () =
  let proof_cache_db = Proof_cache_tag.create_identity_db () in
  let l1_config : Utils.Slot.l1_config =
    let genesis_timestamp =
      Thread_safe.block_on_async_exn (fun () ->
          Gql_client.fetch_genesis_timestamp ~logger l1_uri >>| Or_error.ok_exn )
    in
    { fork_timestamp = genesis_timestamp
    ; fork_slot =
        Thread_safe.block_on_async_exn (fun () ->
            Gql_client.fetch_fork_slot ~logger l1_uri >>| Or_error.ok_exn )
    }
  in
  [%log info] "Current slot: %d"
    ( Utils.Slot.global_slot ~l1_config
    |> Mina_numbers.Global_slot_since_genesis.to_int ) ;
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer.create ~logger ~max_pool_size ~da_config ~da_keys ~da_quorum
          ~db_dir:(Some db_dir) ~checkpoints_dir:(Some checkpoints_dir)
          ~postgres_uri ~l1_uri ~archive_uri
          ~commitment_period_sec:commitment_period ~deposit_delay_blocks
          ?nats_url
          ~signer:
            Signature_lib.(
              Keypair.of_private_key_exn
              @@ Private_key.of_base58_check_exn signer)
          ~mq_host ~fee_modifier ~minimum_fee ~slot_acceptance ~proof_cache_db
          ~l1_config ~commit_validity_period )
  in

  Sequencer.run_committer sequencer ;

  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req -> sequencer)
      (Gql.schema ~proof_cache_db)
  in
  let () =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            [%log error] "Unhandled exception: %s" (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _sock req -> graphql_callback () req body)
    |> Deferred.ignore_m |> don't_wait_for
  in
  [%log info] "Sequencer listening on port %d" port ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko sequencer"
    (let%map_open.Command log_json = Flag.Log.json
     and log_level = Flag.Log.level
     and port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
     and archive_uri =
       flag "--archive-uri" (required string) ~doc:"string archive URI"
     and commitment_period =
       flag "--commitment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 20 int)
         ~doc:"int Maximum transaction pool size"
     and da_nodes =
       flag "--da-node" (listed string)
         ~doc:"string Address of the DA node, can be supplied multiple times"
     and da_keys =
       flag "--da-keys" (required string)
         ~doc:"string List of DA keys, separated by commas"
     and da_quorum =
       flag "--da-quorum" (required int)
         ~doc:"string Quorum for the DA signature count"
     and mq_host =
       flag "--mq-host" (required string)
         ~doc:"string Address of the message queue host"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "db" string)
         ~doc:"string Directory to store the Ledger database"
     and checkpoints_dir =
       flag "--checkpoints-dir"
         (optional_with_default "checkpoints" string)
         ~doc:"string Directory to store the ledger checkpoints"
     and postgres_uri =
       flag "--postgres-uri" (required string) ~doc:"string Postgres URI"
     and deposit_delay_blocks =
       flag "--deposit-delay-blocks"
         (optional_with_default 5 int)
         ~doc:"int Number of blocks to wait before processing deposits"
     and fee_modifier =
       flag "--fee-modifier"
         (optional_with_default 1.0 float)
         ~doc:"float Fee modifier for the sequencer"
     and minimum_fee =
       flag "--minimum-fee"
         (optional_with_default 0.01 float)
         ~doc:"float Minimum fee for the sequencer"
     and nats_url =
       flag "--nats-url" (optional string)
         ~doc:"string Optional NATS URL for explorer event publishing"
     and slot_acceptance_m =
       flag "--slot-acceptance"
         (optional_with_default 60. float)
         ~doc:"float Slot acceptance in minutes"
     and commit_validity_period =
       flag "--commit-validity-period"
         (optional_with_default 20 int)
         ~doc:"int Commit validity period in slots"
     in
     let slot_acceptance = Time.Span.of_min slot_acceptance_m in
     let signer = Sys.getenv_exn "MINA_PRIVATE_KEY" in
     let da_config = Da_layer.Client.Config.of_string_list da_nodes in
     let da_keys =
       String.split ~on:',' da_keys
       |> List.map ~f:Public_key.Compressed.of_base58_check_exn
     in
     let l1_uri = Uri.of_string l1_uri in
     let archive_uri = Uri.of_string archive_uri in
     let nats_url = Option.map nats_url ~f:Uri.of_string in
     let mq_host = Host_and_port.of_string mq_host in
     let logger = Logger.create () in
     let postgres_uri = Uri.of_string postgres_uri in
     let commit_validity_period =
       Mina_numbers.Global_slot_span.of_int commit_validity_period
     in
     Stdout_log.setup log_json log_level ;
     run ~logger ~port ~max_pool_size ~commitment_period ~da_config ~da_keys
       ~da_quorum ~db_dir ~checkpoints_dir ~postgres_uri ~l1_uri ~archive_uri
       ~signer ~deposit_delay_blocks ~mq_host ~fee_modifier ~minimum_fee
       ~nats_url ~slot_acceptance ~commit_validity_period )
  |> Command_unix.run
