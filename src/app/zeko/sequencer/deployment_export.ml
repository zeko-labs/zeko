open Core
open Async
open Mina_base
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Zeko_types
open Sequencer
module Field = Snark_params.Tick.Field
module L = Mina_ledger.Ledger

let constraint_constants = Zeko_constants.constraint_constants

let run = Thread_safe.block_on_async_exn

let write_json path json =
  Out_channel.write_all path ~data:(Yojson.Safe.pretty_to_string json ^ "\n")

let payment ~(sender : Keypair.t) ~receiver ~nonce =
  Signed_command.sign ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
    sender
    { common =
        { fee = Zeko_constants.transaction_fee
        ; fee_payer_pk = Public_key.compress sender.public_key
        ; nonce
        ; valid_until = Mina_numbers.Global_slot_since_genesis.max_value
        ; memo = Signed_command_memo.empty
        }
    ; body =
        Payment
          { receiver_pk = receiver
          ; amount = Currency.Amount.of_mina_string_exn "1"
          }
    }
  |> Signed_command.forget_check

let export ~logger ~output_directory ~db_directory ~l1_uri ~postgres_uri
    ~da_nodes ~da_quorum ~mq_host ~signer_location ~commit_validity_period =
  if
    not
      ( Caml.Sys.file_exists output_directory
      && Caml.Sys.is_directory output_directory )
  then failwithf "Output directory does not exist: %s" output_directory () ;
  if List.is_empty da_nodes then failwith "At least one --da-node is required" ;
  if da_quorum < 1 || da_quorum > List.length da_nodes then
    failwithf "DA quorum %d is invalid for %d nodes" da_quorum
      (List.length da_nodes) () ;
  Unix.putenv ~key:"ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_DIR" ~data:output_directory ;
  Unix.putenv ~key:"ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_ONLY" ~data:"true" ;
  let da_config = Da_layer.Client.Config.of_string_list da_nodes in
  let da_keys =
    run (fun () -> Da_layer.Client.Config.fetch_public_keys ~logger da_config)
  in
  if List.length da_keys <> List.length da_nodes then
    failwithf "Fetched %d DA keys for %d nodes" (List.length da_keys)
      (List.length da_nodes) () ;
  let da_commitment =
    Multisig.commit { public_keys = da_keys; quorum = Field.of_int da_quorum }
  in
  let deploy_config =
    Option.value_exn ~message:"ZEKO_DEPLOY_CONFIG is not set"
      Zeko_circuits_config.deploy_config
  in
  let outer_keypair = Keypair.of_private_key_exn deploy_config.zeko_l1 in
  let holder_keypair =
    Keypair.of_private_key_exn @@ List.hd_exn deploy_config.holder_accounts_l1
  in
  let token_holder_keypair =
    Keypair.of_private_key_exn deploy_config.helper_token_owner_l1
  in
  let signer =
    run (fun () ->
        Signer_service.Client.create ~logger ~location:signer_location
        >>| Signer_service.Signer.of_client )
  in
  let signer_public_key = Signer_service.Signer.public_key signer in
  let signer_keypair =
    Sys.getenv_exn "ZEKO_DEPLOYMENT_SEQUENCER_PRIVATE_KEY"
    |> Private_key.of_base58_check_exn |> Keypair.of_private_key_exn
  in
  if
    not
      (Public_key.Compressed.equal signer_public_key
         (Public_key.compress signer_keypair.public_key) )
  then
    failwith
      "ZEKO_DEPLOYMENT_SEQUENCER_PRIVATE_KEY does not match the signer endpoint" ;
  run (fun () ->
      Gql_client.Local_l1.create_account ~logger l1_uri signer_public_key
      >>| ignore ) ;
  let ( `Inner inner_account
      , `Holder holder_account
      , `Ethereum_asset_registry registry_account ) =
    run Sequencer_lib.Deploy.Z.Inner.initial_accounts
  in
  let funded_balance = Currency.Balance.of_mina_string_exn "100" in
  let signer_account_id =
    Account_id.create signer_public_key Token_id.default
  in
  let signer_account =
    (signer_account_id, Account.create signer_account_id funded_balance)
  in
  let fee_recipient_account_id =
    Account_id.create Zeko_circuits_config.Inputs.bridge_fee_recipient_l2
      Token_id.default
  in
  let fee_recipient_account =
    ( fee_recipient_account_id
    , Account.create fee_recipient_account_id Currency.Balance.zero )
  in
  let genesis_accounts =
    [ (Account.identifier inner_account, inner_account)
    ; (Account.identifier holder_account, holder_account)
    ]
    @ ( Option.to_list registry_account
      |> List.map ~f:(fun account -> (Account.identifier account, account)) )
    @ [ signer_account; fee_recipient_account ]
  in
  let genesis_ledger =
    L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
  in
  List.iter genesis_accounts ~f:(fun (account_id, account) ->
      L.create_new_account_exn genesis_ledger account_id account ) ;
  let account_set_hash =
    let db =
      Indexed_merkle_tree.Db.create ~depth:constraint_constants.ledger_depth ()
    in
    List.iter genesis_accounts ~f:(fun (account_id, _) ->
        let token_id = Account_id.derive_token_id ~owner:account_id in
        let _, _ = Indexed_merkle_tree.Db.get_or_create_entry_exn db token_id in
        () ) ;
    Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root db |]
  in
  run (fun () ->
      Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
        ~ledger:genesis_ledger ~get_actions_for_aid:(fun _ -> []) ) ;
  run (fun () ->
      let%bind nonce =
        Gql_client.infer_nonce ~logger l1_uri signer_public_key
        >>| Or_error.ok_exn
      in
      let%bind command =
        Sequencer_lib.Deploy.deploy_command_exn
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~signer_pk:signer_public_key
          ~outer_pk:(Public_key.compress outer_keypair.public_key)
          ~holder_pk:(Public_key.compress holder_keypair.public_key)
          ~token_holder_pk:(Public_key.compress token_holder_keypair.public_key)
          ~fee:Zeko_constants.outer_account_creation_fee ~nonce
          ~initial_ledger:genesis_ledger
          ~account_creation_fee:Zeko_constants.account_creation_fee
          ~account_set_hash
          ~pause_key:(Even_PC.create_exn signer_public_key)
          ~sequencer:(Even_PC.create_exn signer_public_key)
          ~da_key:da_commitment ~prefund_amount:Currency.Amount.zero ()
      in
      let command =
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 command
          [ outer_keypair
          ; holder_keypair
          ; token_holder_keypair
          ; signer_keypair
          ]
      in
      let%bind _ =
        Gql_client.send_zkapp l1_uri
          (Zkapp_command.read_all_proofs_from_disk command)
        >>| function
        | Ok response ->
            response
        | Error (`Failed_request error | `Graphql_error error) ->
            failwith error
      in
      let%map _ = Gql_client.Local_l1.create_new_block ~logger l1_uri in
      () ) ;
  let genesis_timestamp =
    run (fun () ->
        Gql_client.fetch_genesis_timestamp ~logger l1_uri >>| Or_error.ok_exn )
  in
  let l1_config : Utils.Slot.l1_config =
    { fork_timestamp = genesis_timestamp
    ; fork_slot = Mina_numbers.Global_slot_since_genesis.zero
    ; slot_duration_sec = 180
    }
  in
  let sequencer =
    run (fun () ->
        Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
          ~da_config ~da_keys ~da_quorum ~db_dir:(Some db_directory)
          ~checkpoints_dir:None ~postgres_uri ~l1_uri ~archive_uri:l1_uri
          ~signer ~deposit_delay_blocks:0 ~mq_host ~fee_modifier:1.0
          ~minimum_fee:Zeko_constants.minimum_fee
          ~slot_acceptance:(Time.Span.of_min 10.)
          ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
          ~l1_config
          ~commit_validity_period:
            (Mina_numbers.Global_slot_span.of_int commit_validity_period)
          ~commit_fee:Zeko_constants.transaction_fee
          ~bridge_txn_fee:Zeko_constants.transaction_fee )
  in
  let outer_public_key = Public_key.compress outer_keypair.public_key in
  let outer_action_state =
    run (fun () ->
        Gql_client.fetch_action_state l1_uri outer_public_key ~logger
        >>| Or_error.ok_exn )
  in
  let genesis_ledger_json =
    L.to_list_sequential genesis_ledger
    |> List.mapi ~f:(fun index account -> (index, account))
    |> [%to_yojson: (int * Account.t) list]
  in
  let genesis_ledger_hash = L.merkle_root genesis_ledger in
  write_json
    (Filename.concat output_directory "genesis-ledger.json")
    genesis_ledger_json ;
  write_json
    (Filename.concat output_directory "deployment-manifest.json")
    (`Assoc
      [ ("schemaVersion", `Int 1)
      ; ("commitValidityPeriod", `Int commit_validity_period)
      ; ( "outerPublicKey"
        , `String (Public_key.Compressed.to_base58_check outer_public_key) )
      ; ( "outerActionState"
        , `String (Ethereum_settlement_export.field_to_hex outer_action_state)
        )
      ; ( "genesisLedgerHash"
        , `String (Ledger_hash.to_base58_check genesis_ledger_hash) )
      ; ( "genesisLedgerField"
        , `String
            (Ethereum_settlement_export.field_to_hex
               (genesis_ledger_hash :> Field.t) ) )
      ; ( "sequencerPublicKey"
        , `String (Public_key.Compressed.to_base58_check signer_public_key) )
      ; ( "daPublicKeys"
        , `List
            (List.map da_keys ~f:(fun public_key ->
                 `String (Public_key.Compressed.to_base58_check public_key) ) )
        )
      ; ("daNodeCount", `Int (List.length da_keys))
      ; ("daQuorum", `Int da_quorum)
      ; ( "daCommitment"
        , `String (Ethereum_settlement_export.field_to_hex da_commitment) )
      ] ) ;
  let submit_payment () =
    let nonce =
      Sequencer.infer_nonce sequencer
        (Public_key.compress signer_keypair.public_key)
    in
    run (fun () ->
        Sequencer.apply_user_command sequencer
          (Signed_command
             (payment ~sender:signer_keypair
                ~receiver:Zeko_circuits_config.Inputs.bridge_fee_recipient_l2
                ~nonce ) )
        >>| Or_error.ok_exn )
  in
  submit_payment () ;
  submit_payment () ;
  run (fun () ->
      let%bind commit_result = Sequencer.commit sequencer >>| Or_error.ok_exn in
      let%bind settlement = commit_result >>| Or_error.ok_exn in
      if Option.is_none settlement then failwith "Deployment commit was empty" ;
      let%bind () = Executor.wait_to_finish sequencer.merger_ctx.executor in
      let%bind _ = Gql_client.Local_l1.create_new_block ~logger l1_uri in
      let%map { ledger_hash = committed_ledger_hash; _ } =
        Gql_client.infer_state ~logger l1_uri ~signer_pk:signer_public_key
          ~zkapp_pk:outer_public_key
        >>| Or_error.ok_exn
        >>| Utils.value_of_zkapp_state
              Zeko_circuits.Rollup_state.Outer_state.typ
      in
      let target_ledger_hash = Sequencer.get_root sequencer in
      if not (Ledger_hash.equal committed_ledger_hash target_ledger_hash) then
        failwith "Exported settlement did not commit the sequencer ledger" ) ;
  run (fun () -> Sequencer.shutdown sequencer)

let command =
  Command.basic ~summary:"Export proof-bound Ethereum deployment artifacts"
    (let%map_open.Command output_directory =
       flag "--output-directory" (required string)
         ~doc:"path Existing artifact output directory"
     and l1_uri =
       flag "--l1-uri" (required string) ~doc:"URI Local Mina GraphQL endpoint"
     and db_directory =
       flag "--db-dir" (required string)
         ~doc:"path Dedicated sequencer database directory"
     and postgres_uri =
       flag "--postgres-uri" (required string)
         ~doc:"URI Dedicated sequencer PostgreSQL database"
     and da_nodes = flag "--da-node" (listed string) ~doc:"host:port DA node"
     and da_quorum =
       flag "--da-quorum" (required int) ~doc:"int DA signature quorum"
     and mq_host =
       flag "--mq-host" (required string) ~doc:"host:port Prover message queue"
     and signer_location =
       flag "--signer" (required string)
         ~doc:"host:port Sequencer signer endpoint"
     and commit_validity_period =
       flag "--commit-validity-period"
         (optional_with_default 20 int)
         ~doc:"slots Settlement validity period"
     and log_json = Cli_lib.Flag.Log.json
     and log_level = Cli_lib.Flag.Log.level in
     fun () ->
       Cli_lib.Stdout_log.setup log_json log_level ;
       let logger = Logger.create () in
       export ~logger ~output_directory ~db_directory
         ~l1_uri:(Uri.of_string l1_uri)
         ~postgres_uri:(Uri.of_string postgres_uri)
         ~da_nodes ~da_quorum
         ~mq_host:(Host_and_port.of_string mq_host)
         ~signer_location:(Host_and_port.of_string signer_location)
         ~commit_validity_period )

let () = Command_unix.run command
