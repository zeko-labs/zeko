open Core
open Async
open Sequencer_lib
open Cli_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)
module Sequencer = Zeko_sequencer.Sequencer

let run ~logger ~port ~max_pool_size ~commitment_period ~da_config ~da_keys
    ~da_quorum ~db_dir ~checkpoints_dir ~postgres_uri ~l1_uri ~archive_uri
    ~signer ~deposit_delay_blocks ~mq_host ~fee_modifier ~minimum_fee
    ~slot_acceptance ~slot_duration_sec ~commit_validity_period ~commit_fee
    ~bridge_txn_fee ~inner_sync_period () =
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
    ; slot_duration_sec
    }
  in
  [%log info] "Current slot: %d"
    ( Utils.Slot.global_slot ~l1_config
    |> Mina_numbers.Global_slot_since_genesis.to_int ) ;
  let signer =
    Thread_safe.block_on_async_exn (fun () ->
        Signer_service.Client.create ~logger
          ~location:(Host_and_port.of_string signer)
        >>| Signer_service.Signer.of_client )
  in
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer.create ~logger ~max_pool_size ~da_config ~da_keys ~da_quorum
          ~db_dir:(Some db_dir) ~checkpoints_dir:(Some checkpoints_dir)
          ~postgres_uri ~l1_uri ~archive_uri
          ~commitment_period_sec:commitment_period ~deposit_delay_blocks ~signer
          ~mq_host ~fee_modifier ~minimum_fee ~slot_acceptance ~proof_cache_db
          ~l1_config ~commit_validity_period ~commit_fee ~bridge_txn_fee )
  in

  Sequencer.run_committer sequencer ;
  Sequencer.run_inner_syncer sequencer ~period_sec:inner_sync_period ;

  let l2_executor =
    Executor.create
      ~kind:
        (`L2
          { infer_nonce = Sequencer.infer_nonce sequencer
          ; apply_user_command = Sequencer.apply_user_command sequencer
          } )
      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 ~signer ()
  in

  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req ->
        Gql.Context.
          { sequencer
          ; l1_executor = sequencer.merger_ctx.executor
          ; l2_executor
          } )
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
     and inner_sync_period =
       flag "--inner-sync-period"
         (optional_with_default 0. float)
         ~doc:
           "float Period in seconds for synchronizing commit-only outer action \
            batches without emitting a commit (0 disables)"
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
         (optional_with_default Zeko_constants.minimum_fee float)
         ~doc:"float Minimum fee for the sequencer"
     and slot_acceptance_m =
       flag "--slot-acceptance"
         (optional_with_default 60. float)
         ~doc:"float Slot acceptance in minutes"
     and slot_duration_sec =
       flag "--slot-duration"
         (optional_with_default 180 int)
         ~doc:"int L1 slot duration in seconds"
     and commit_validity_period =
       flag "--commit-validity-period"
         (optional_with_default 20 int)
         ~doc:"int Commit validity period in slots"
     and signer =
       flag "--signer" (required string) ~doc:"string Signer service host:port"
     and commit_fee =
       flag "--commit-fee"
         (optional_with_default Zeko_constants.transaction_fee_string string)
         ~doc:"string Commit fee in native units"
     and bridge_txn_fee =
       flag "--bridge-txn-fee"
         (optional_with_default Zeko_constants.transaction_fee_string string)
         ~doc:"string Bridge transaction fee in native units"
     in
     let slot_acceptance = Time.Span.of_min slot_acceptance_m in
     if slot_duration_sec <= 0 then
       failwith "--slot-duration must be a positive number of seconds" ;
     let da_config = Da_layer.Client.Config.of_string_list da_nodes in
     let da_keys =
       String.split ~on:',' da_keys
       |> List.map ~f:Signature_lib.Public_key.Compressed.of_base58_check_exn
     in
     let l1_uri = Uri.of_string l1_uri in
     let archive_uri = Uri.of_string archive_uri in
     let mq_host = Host_and_port.of_string mq_host in
     let logger = Logger.create () in
     let postgres_uri = Uri.of_string postgres_uri in
     let commit_validity_period =
       Mina_numbers.Global_slot_span.of_int commit_validity_period
     in
     let commit_fee = Currency.Fee.of_mina_string_exn commit_fee in
     let bridge_txn_fee = Currency.Fee.of_mina_string_exn bridge_txn_fee in
     Stdout_log.setup log_json log_level ;
     run ~logger ~port ~max_pool_size ~commitment_period ~da_config ~da_keys
       ~da_quorum ~db_dir ~checkpoints_dir ~postgres_uri ~l1_uri ~archive_uri
       ~signer ~deposit_delay_blocks ~mq_host ~fee_modifier ~minimum_fee
       ~slot_acceptance ~slot_duration_sec ~commit_validity_period ~commit_fee
       ~bridge_txn_fee ~inner_sync_period )
  |> Command_unix.run
