open Core_kernel
open Async
open Mina_base
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Zeko_types
open Sequencer
open Test_spec
open Handle.Operator
open Unsigned_extended
module Field = Snark_params.Tick.Field

let constraint_constants = Zeko_constants.constraint_constants

let start_time = Time.now ()

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Spam ;
  Logger.create ()

let gql_uri = Uri.of_string "http://localhost:8080/graphql"

let da_config_with2 =
  Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555"; "127.0.0.1:8556" ]

let da_config_with3 =
  Da_layer.Client.Config.of_string_list
    [ "127.0.0.1:8555"; "127.0.0.1:8556"; "127.0.0.1:8557" ]

let da_keys =
  run (fun () ->
      Da_layer.Client.Config.fetch_public_keys ~logger da_config_with3 )

let da_quorum = 2

let mq_host = Host_and_port.of_string "localhost:5672"

let run = Thread_safe.block_on_async_exn

let get_test_signer () =
  run (fun () ->
      let location =
        Sys.getenv_exn "ZEKO_TEST_SEQUENCER_SIGNER" |> Host_and_port.of_string
      in
      Signer_service.Client.create ~logger ~location
      >>| Signer_service.Signer.of_client )

let free_sequencer (sequencer : Sequencer.t Handle.valid_t) =
  Gc.full_major () ;
  run (fun () -> Sequencer.shutdown !sequencer) ;
  Handle.invalidate sequencer

let slot_acceptance = Time.Span.of_min 60.

let () =
  print_endline "Started test 'apply commands and commit'" ;

  let postgres_uri1 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer1" )
  in
  let postgres_uri2 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer2" )
  in

  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:5
       ~postgres_uri:postgres_uri1 ~gql_uri ~da_config:da_config_with2 ~da_keys
       ~da_quorum ~mq_host ~slot_acceptance () )
    ~f:(fun
         { outer_kp
         ; signer_pk
         ; specs
         ; sequencer
         ; da_keys
         ; accounts
         ; l1_config
         ; _
         }
       ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      let zkapp_command_with_real_proof =
        let fee_signer = List.hd_exn accounts in
        let account_creation_fee =
          Account_update.with_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress fee_signer.public_key
              ; balance_change =
                  Currency.Amount.Signed.of_fee @@ Currency.Fee.Signed.negate
                  @@ Currency.Fee.Signed.of_unsigned
                       constraint_constants.account_creation_fee
              ; authorization_kind = Signature
              ; use_full_commitment = true
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        in
        let open Initialize_state.Test_module in
        (* First one is the inner account *)
        let call_forest =
          Zkapp_command.Call_forest.cons
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
            account_creation_fee
          @@ Zkapp_command.Call_forest.cons
               ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
               Deploy_account_update.account_update
          @@ Zkapp_command.Call_forest.cons_tree
               Initialize_account_update.account_update
          @@ Zkapp_command.Call_forest.cons_tree
               Update_state_account_update.account_update []
        in
        User_command.Zkapp_command
          (Utils.sign_zkapp_command
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
             { fee_payer =
                 { body =
                     { Account_update.Body.Fee_payer.dummy with
                       public_key = Public_key.compress fee_signer.public_key
                     ; fee = Currency.Fee.of_mina_int_exn 1
                     }
                 ; authorization = Signature.dummy
                 }
             ; account_updates = call_forest
             ; memo = Signed_command_memo.empty
             }
             [ fee_signer; Keypair.of_private_key_exn sk ] )
      in
      let batch1, batch2 = List.split_n commands 1 in
      let batch1 = zkapp_command_with_real_proof :: batch1 in
      let batch2, batch3 = List.split_n batch2 2 in

      print_endline "(* Apply first batch *)" ;
      run (fun () ->
          Deferred.List.iter batch1 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* First commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

          Deferred.unit ) ;

      (* To test nonce inferring from pool *)
      (* The first commit is still in the pool *)
      Executor.refresh_nonce !sequencer.merger_ctx.executor ;

      print_endline "(* Apply second batch *)" ;
      run (fun () ->
          Deferred.List.iter batch2 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Second commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Apply third batch *)" ;
      run (fun () ->
          Deferred.List.iter batch3 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Third commit *)" ;
      let final_ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            let%bind { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root !sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

            return target_ledger_hash )
      in

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Try to bootstrap again *)" ;
      let new_sequencer =
        run (fun () ->
            let%map new_sequencer =
              Sequencer.create ~logger ~max_pool_size:10
                ~commitment_period_sec:0. ~da_config:da_config_with2 ~da_keys
                ~da_quorum ~db_dir:None ~checkpoints_dir:None
                ~postgres_uri:postgres_uri2 ~l1_uri:gql_uri ~archive_uri:gql_uri
                ~signer:(get_test_signer ()) ~deposit_delay_blocks:0 ~mq_host
                ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
                ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
                ~l1_config
                ~commit_validity_period:
                  (Mina_numbers.Global_slot_span.of_int 10)
            in
            [%test_eq: Frozen_ledger_hash.t] (get_root new_sequencer)
              final_ledger_hash ;
            new_sequencer )
      in

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer1" ) ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer2" ) )

let () =
  print_endline "Started test 'dummy signature should fail'" ;

  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in

  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () ) ~f:(fun { specs; sequencer; _ } ->
      let dummy_signature_command : Zkapp_command.t =
        let command = account_update_send (List.hd_exn specs) in
        { command with
          account_updates =
            Zkapp_command.Call_forest.map command.account_updates
              ~f:(fun account_update ->
                match Account_update.Poly.authorization account_update with
                | Control.Poly.Signature _ ->
                    { account_update with
                      authorization = Control.Poly.Signature Signature.dummy
                    }
                | _ ->
                    account_update )
        }
      in
      let result =
        run (fun () ->
            apply_user_command !sequencer (Zkapp_command dummy_signature_command) )
      in
      match result with
      | Error e
        when String.is_substring ~substring:"Invalid_signature"
               (Error.to_string_hum e) ->
          run (fun () ->
              Gc.full_major () ;
              let%bind () = Sequencer.shutdown !sequencer in
              Relational_db.For_tests.drop_database ~port:5433 "sequencer" )
      | Ok _ ->
          failwith "Transaction should have failed"
      | Error unexpected_error ->
          Error.raise unexpected_error )

let () =
  print_endline "Started test 'restart sequencer and requeue witnesses'" ;
  let db_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      run (fun () ->
          Deferred.List.iter commands ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with3 ~da_quorum ~db_dir:(Some db_dir)
              ~checkpoints_dir:None ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer:(get_test_signer ())
              ~deposit_delay_blocks:0 ~mq_host ~da_keys ~fee_modifier:1.0
              ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10) )
      in

      print_endline "(* Requeue witnesses and commit with quorum 3 *)" ;
      let ledger_hash =
        run (fun () ->
            let%bind commit_result = commit new_sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish new_sequencer.merger_ctx.executor
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root new_sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;
            committed_ledger_hash )
      in

      print_endline "(* Check that all da nodes are synced *)" ;
      run (fun () ->
          let%bind _multisig =
            Da_layer.Client.get_multisig
              { new_sequencer.da_client with quorum = 3 }
              ~ledger_hash
          in
          let%map da_nodes_synced =
            Deferred.List.map da_config_with3.nodes ~f:(fun node ->
                Da_layer.Client.Rpc.has_diff ~logger ~node_location:node
                  ~ledger_hash )
            >>| Result.all >>| Or_error.ok_exn >>| List.for_all ~f:Fn.id
          in
          [%test_eq: bool] da_nodes_synced true ) ;

      print_endline "(* Assert that no witnesses are left in merger *)" ;
      run (fun () ->
          let%map all_witnesses =
            Relational_db.Pool.use
              (fun conn -> Merger.P.Witness_table.get_all conn ())
              new_sequencer.db_pool
            >>| Relational_db.caqti_ok_exn
                  ~msg:"Failed to get all witnesses: %s"
          in
          [%test_eq: int] (List.length all_witnesses) 0 ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'create checkpoints and restart from checkpoint'" ;
  let db_dir1 =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let db_dir2 =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let checkpoints_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri1 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer1" )
  in
  let postgres_uri2 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer2" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir:db_dir1 ~checkpoints_dir
       ~postgres_uri:postgres_uri1 ~gql_uri ~da_config:da_config_with2 ~da_keys
       ~da_quorum ~mq_host ~slot_acceptance () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      run (fun () ->
          Deferred.List.iter commands ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Commit *)" ;
      let ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root !sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;
            committed_ledger_hash )
      in

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer from checkpoint *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with3 ~da_quorum ~db_dir:(Some db_dir2)
              ~checkpoints_dir:(Some checkpoints_dir)
              ~postgres_uri:postgres_uri2 ~l1_uri:gql_uri ~archive_uri:gql_uri
              ~signer:(get_test_signer ()) ~deposit_delay_blocks:0 ~mq_host
              ~da_keys ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10) )
      in

      print_endline "(* Check that all da nodes are synced *)" ;
      run (fun () ->
          let%bind _multisig =
            Da_layer.Client.get_multisig
              { new_sequencer.da_client with quorum = 3 }
              ~ledger_hash
          in
          let%map da_nodes_synced =
            Deferred.List.map da_config_with3.nodes ~f:(fun node ->
                Da_layer.Client.Rpc.has_diff ~logger ~node_location:node
                  ~ledger_hash )
            >>| Result.all >>| Or_error.ok_exn >>| List.for_all ~f:Fn.id
          in
          [%test_eq: bool] da_nodes_synced true ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer1" ) ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer2" ) )

let () =
  print_endline "Started test 'restart sequencer and recommit'" ;
  let db_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      let batch1, batch2 = List.split_n commands 3 in
      let initial_ledger_hash = get_root !sequencer in

      print_endline "(* Apply first batch *)" ;
      run (fun () ->
          Deferred.List.iter batch1 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* First commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          Executor.wait_to_finish !sequencer.merger_ctx.executor ) ;

      print_endline "(* Apply second batch *)" ;
      run (fun () ->
          Deferred.List.iter batch2 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Second commit *)" ;
      let final_ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _cleared =
              Gql_client.For_tests.clear_pool ~logger gql_uri
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            [%test_eq: Ledger_hash.t] committed_ledger_hash initial_ledger_hash ;
            get_root !sequencer )
      in

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with2 ~da_quorum ~db_dir:(Some db_dir)
              ~checkpoints_dir:None ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer:(get_test_signer ())
              ~deposit_delay_blocks:0 ~mq_host ~da_keys ~fee_modifier:1.0
              ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10) )
      in

      print_endline "(* Check that after restart it recommited *)" ;
      run (fun () ->
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          [%test_eq: Ledger_hash.t] committed_ledger_hash final_ledger_hash ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'slot range check'" ;
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:5 ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host
       ~slot_acceptance:(Time.Span.of_min 10.) () )
    ~f:(fun { specs; sequencer; signer_pk; outer_kp; l1_config; _ } ->
      run (fun () ->
          let open Mina_numbers in
          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Signed_command
              (command_send
                 ~valid_until:
                   Global_slot_since_genesis.(
                     add current_slot (Global_slot_span.of_int 1))
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error
               (Error.of_string
                  "Upper slot has too small margin to be committed" ) ) ;
          let command =
            User_command.Signed_command
              (command_send
                 ~valid_until:
                   Global_slot_since_genesis.(
                     add current_slot (Global_slot_span.of_int 5))
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result (Ok ()) ;

          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~global_slot_precondition:
                   ( Check
                       { lower = current_slot
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 1))
                       }
                   , Check
                       { lower =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 2))
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 3))
                       } )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error (Error.of_string "Conflicting slot ranges")) ;

          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~valid_while:
                   ( Check
                       { lower =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 1))
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 10))
                       }
                   , Ignore )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error (Error.of_string "Lower slot is in the future")) ;

          let spec, _specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~valid_until:
                   (Some
                      Global_slot_since_genesis.(
                        add current_slot (Global_slot_span.of_int 10)) )
                 ~valid_while:
                   ( Check
                       { lower = current_slot
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 15))
                       }
                   , Ignore )
                 ~global_slot_precondition:
                   ( Ignore
                   , Check
                       { lower =
                           Global_slot_since_genesis.(
                             sub current_slot (Global_slot_span.of_int 15)
                             |> Option.value_exn)
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 20))
                       } )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result (Ok ()) ;

          return () ) ;

      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'deposits'" ;
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  let open Mina_numbers in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:0 ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host
       ~slot_acceptance:(Time.Span.of_min 10.)
       ~commit_validity_period:(Global_slot_span.of_int 20)
       () )
    ~f:(fun { outer_kp; sequencer; signer_pk; l1_config; _ } ->
      (* Create l1 accounts *)
      let l1_accounts =
        Array.create ~len:6 ()
        |> Array.map ~f:Signature_lib.Keypair.create
        |> Array.to_list
      in
      run (fun () ->
          Deferred.List.iter l1_accounts ~f:(fun keypair ->
              let%bind _res =
                Gql_client.For_tests.create_account ~logger gql_uri
                  (Signature_lib.Public_key.compress keypair.public_key)
              in
              return () ) ) ;

      let submit_deposit ~fee (signer : Keypair.t)
          (deposit_params : C.Bridge_state.Deposit_params_base.t) =
        let%bind nonce =
          Gql_client.fetch_nonce ~logger gql_uri
            (Signature_lib.Public_key.compress signer.public_key)
          >>| Or_error.ok_exn
        in
        let fee_payer =
          Account_update.Fee_payer.
            { body =
                { public_key = Public_key.compress signer.public_key
                ; fee = Currency.Fee.of_mina_int_exn fee
                ; valid_until = None
                ; nonce = Account.Nonce.of_uint32 nonce
                }
            ; authorization = Signature.dummy
            }
        in
        let bridge_proof_fee = Zeko_circuits_config.Inputs.bridge_proof_fee in
        let transferrer_update =
          Account_update.with_no_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress signer.public_key
              ; balance_change =
                  Currency.Amount.Signed.(
                    negate @@ of_unsigned
                    @@ Option.value_exn
                         (Currency.Amount.add deposit_params.amount
                            bridge_proof_fee ))
              ; use_full_commitment = false
              ; increment_nonce = true
              ; authorization_kind = Signature
              ; preconditions =
                  { Account_update.Preconditions.accept with
                    account =
                      Zkapp_precondition.Account.nonce
                        (Account.Nonce.succ nonce)
                  }
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        in
        let%map transfer_forest =
          Bridge_prover.(
            prove !sequencer.bridge_prover
              (Deposit_request.f ~logger
                 { deposit_params; transferrer = transferrer_update } ))
          >>| Or_error.ok_exn
        in
        let transfer_cmd : Zkapp_command.t =
          { fee_payer
          ; account_updates =
              transfer_forest
              |> Zkapp_command.Call_forest.map
                   ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
              |> Utils.rehash_forest
                   ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ; memo = Signed_command_memo.empty
          }
        in
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 transfer_cmd
          [ signer ]
        |> Zkapp_command.read_all_proofs_from_disk
      in
      let deposit ~amount ~(account : Keypair.t) ~timeout :
          C.Bridge_state.Deposit_params_base.t =
        let current_slot : Global_slot_since_genesis.t =
          Utils.Slot.global_slot ~l1_config
        in
        let timeout =
          C.Zeko_util.Slot.add current_slot (Global_slot_span.of_int timeout)
        in
        { children = []
        ; holder_account_l1 =
            List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
        ; recipient = Public_key.compress account.public_key
        ; amount = Currency.Amount.of_mina_int_exn amount
        ; timeout
        }
      in

      let account1 = List.nth_exn l1_accounts 0 in
      let account2 = List.nth_exn l1_accounts 1 in
      let account3 = List.nth_exn l1_accounts 2 in

      print_endline "(* Send 1-3 deposits *)" ;
      let deposits =
        run (fun () ->
            let deposit1 = deposit ~amount:10 ~account:account1 ~timeout:40 in
            let deposit2 = deposit ~amount:20 ~account:account2 ~timeout:40 in
            let deposit3 = deposit ~amount:30 ~account:account3 ~timeout:40 in

            let%bind _ =
              submit_deposit ~fee:6 account1 deposit1
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _ =
              submit_deposit ~fee:5 account2 deposit2
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _ =
              submit_deposit ~fee:4 account3 deposit3
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            return
              [ (account1, deposit1)
              ; (account2, deposit2)
              ; (account3, deposit3)
              ] )
      in

      print_endline "(* Commit 1-3 deposits *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Send 4-6 deposits *)" ;
      let deposits =
        deposits
        @ run (fun () ->
              let deposit4 = deposit ~amount:40 ~account:account1 ~timeout:40 in
              let deposit5 = deposit ~amount:50 ~account:account2 ~timeout:40 in
              let deposit6 = deposit ~amount:60 ~account:account3 ~timeout:40 in

              let%bind _ =
                submit_deposit ~fee:3 account1 deposit4
                >>= Gql_client.send_zkapp gql_uri
              in
              let%bind _ =
                submit_deposit ~fee:2 account2 deposit5
                >>= Gql_client.send_zkapp gql_uri
              in
              let%bind _ =
                submit_deposit ~fee:1 account3 deposit6
                >>= Gql_client.send_zkapp gql_uri
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block ~logger gql_uri
              in
              return
                [ (account1, deposit4)
                ; (account2, deposit5)
                ; (account3, deposit6)
                ] )
      in

      print_endline "(* Commit 4-6 deposits *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Sync the latest commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let finalize_deposit (signer : Keypair.t) deposit_params =
        let fee_payer =
          Account_update.Fee_payer.
            { body =
                { public_key = Public_key.Compressed.empty
                ; fee = Currency.Fee.zero
                ; valid_until = None
                ; nonce = Account.Nonce.zero
                }
            ; authorization = Signature.dummy
            }
        in
        let%bind actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( my_deposit_index
            , (_my_deposit, `Before before_my_deposit_action_state, _) ) =
          let hashed_deposit =
            Utils.value_to_hash ~init:Zeko_constants.deposit_salt
              C.Bridge_state.Deposit_params_base.typ deposit_params
          in
          List.findi actions ~f:(fun _ (action, _, _) ->
              match action with
              | Commit _ ->
                  false
              | Witness witness ->
                  Field.equal hashed_deposit witness.aux )
          |> Option.value_exn ~message:"Did not find my deposit"
        in
        let ( nearest_commit_index
            , (_nearest_commit, _, `After after_nearest_commit_action_state) ) =
          List.sub actions ~pos:my_deposit_index
            ~len:(List.length actions - my_deposit_index)
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find nearest commit"
        in
        let nearest_commit_index = nearest_commit_index + my_deposit_index in
        let check_accepted :
            Bridge.Check_accepted_mina.Init.t
            * Bridge.Check_accepted_mina.Elem.t list =
          ( { params = deposit_params
            ; original_action_state =
                C.Rollup_state.Outer_action_state.unsafe_value_of_field
                  before_my_deposit_action_state
            ; deposit_index = UInt32.of_int my_deposit_index
            }
          , List.sub actions ~pos:my_deposit_index
              ~len:(nearest_commit_index - my_deposit_index + 1)
            |> List.map ~f:(fun (action, _, _) -> action) )
        in
        let current_synced_outer_action_state =
          Sequencer.current_synced_outer_action_state !sequencer
        in
        let%bind ase_actions =
          Gql_client.fetch_actions ~logger gql_uri
            ~from_action_state:after_nearest_commit_action_state
            ~end_action_state:
              C.Rollup_state.Outer_action_state.(
                With_length.state current_synced_outer_action_state |> raw)
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, _, _) ->
                  Zkapp_account.Actions_impl.hash fields )
        in
        let ase : Ase.With_length.Stmt.t * Field.t list =
          ( { action_state = after_nearest_commit_action_state
            ; length =
                Zeko_circuits.Zeko_util.(
                  Checked32.sub
                    C.Rollup_state.Outer_action_state.(
                      With_length.length current_synced_outer_action_state)
                    (Checked32.of_int @@ List.length ase_actions)
                  |> Option.value_exn ~message:"Negative length")
            }
          , ase_actions )
        in
        let helper_account =
          Sequencer.get_account !sequencer
            (Public_key.compress signer.public_key)
            (Account_id.derive_token_id
               ~owner:
                 (Account_id.of_public_key
                    (Public_key.decompress_exn
                       Zeko_circuits_config.Inputs.holder_account_l2 ) ) )
        in
        let prev_next_deposit =
          Option.value ~default:UInt32.zero
            (let%bind.Option acc = helper_account in
             let%map.Option zkapp = Account.zkapp acc in
             let (next_deposit :: _ : F.t Zkapp_state.V.t) =
               Zkapp_account.Poly.app_state zkapp
             in
             UInt32.of_string (Field.to_string next_deposit) )
        in
        let prev_nonce =
          Option.value ~default:UInt32.zero
            (Option.map helper_account ~f:(fun acc ->
                 UInt32.of_string
                   (Mina_numbers.Account_nonce.to_string acc.nonce) ) )
        in
        let%map transfer_forest =
          Bridge_prover.(
            prove !sequencer.bridge_prover
              (Finalize_deposit.f ~logger
                 { ase_source = fst ase
                 ; ase_elems = snd ase
                 ; check_accepted_init = fst check_accepted
                 ; check_accepted_elems = snd check_accepted
                 ; prev_next_deposit
                 ; prev_nonce
                 ; helper_account_new = Option.is_none helper_account
                 } ))
          >>| Or_error.ok_exn
        in
        let transfer_cmd : Zkapp_command.t =
          { fee_payer
          ; account_updates =
              transfer_forest
              |> Zkapp_command.Call_forest.map
                   ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
          ; memo = Signed_command_memo.empty
          }
        in
        printf "transfer_cmd: %s\n%!"
          (Zkapp_command.to_yojson transfer_cmd |> Yojson.Safe.pretty_to_string) ;
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 transfer_cmd
          [ signer ]
      in

      print_endline "(* Finalize all deposits *)" ;
      run (fun () ->
          Deferred.List.iteri deposits ~f:(fun i (signer, deposit_params) ->
              printf "(* Finalizing deposit %d *)\n%!" i ;
              let%bind command = finalize_deposit signer deposit_params in
              let%map result =
                apply_user_command !sequencer (Zkapp_command command)
              in
              [%test_eq: unit Or_error.t] result (Ok ()) ) ) ;

      print_endline "(* Send 7-9 deposits *)" ;
      let timeout_deposits =
        run (fun () ->
            let deposit7 = deposit ~amount:70 ~account:account1 ~timeout:10 in
            let deposit8 = deposit ~amount:80 ~account:account2 ~timeout:10 in
            let deposit9 = deposit ~amount:90 ~account:account3 ~timeout:10 in

            let%bind _ =
              submit_deposit ~fee:3 account1 deposit7
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _ =
              submit_deposit ~fee:2 account2 deposit8
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _ =
              submit_deposit ~fee:1 account3 deposit9
              >>= Gql_client.send_zkapp gql_uri
            in
            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            return
              [ (account1, deposit7)
              ; (account2, deposit8)
              ; (account3, deposit9)
              ] )
      in

      (* deposit timeout has to be more than commit validity period *)
      (* canceled deposit timeout has to be less than commit validity period *)
      (* shift has to be more than timeout but less than commit validity period for canceled deposit *)
      print_endline "(* Commit 7-9 deposits after timeout *)" ;
      run (fun () ->
          let%bind _shifted =
            Gql_client.For_tests.shift_slots ~logger gql_uri 15
          in
          Utils.Slot.For_tests.add_to_global_slot := 15 ;
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let cancel_deposit ~fee (signer : Keypair.t) deposit_params =
        let%bind nonce =
          Gql_client.fetch_nonce ~logger gql_uri
            (Signature_lib.Public_key.compress signer.public_key)
          >>| Or_error.ok_exn
        in
        let fee_payer =
          Account_update.Fee_payer.
            { body =
                { public_key = Public_key.compress signer.public_key
                ; fee = Currency.Fee.of_mina_int_exn fee
                ; valid_until = None
                ; nonce = Account.Nonce.of_uint32 nonce
                }
            ; authorization = Signature.dummy
            }
        in
        let%bind actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( my_deposit_index
            , (_my_deposit, `Before before_my_deposit_action_state, _) ) =
          let hashed_deposit =
            Utils.value_to_hash ~init:Zeko_constants.deposit_salt
              C.Bridge_state.Deposit_params_base.typ deposit_params
          in
          List.findi actions ~f:(fun _ (action, _, _) ->
              match action with
              | Commit _ ->
                  false
              | Witness witness ->
                  Field.equal hashed_deposit witness.aux )
          |> Option.value_exn ~message:"Did not find my deposit"
        in
        let ( nearest_commit_index
            , ( nearest_commit
              , `Before before_nearest_commit_action_state
              , `After after_nearest_commit_action_state ) ) =
          List.sub actions ~pos:my_deposit_index
            ~len:(List.length actions - my_deposit_index)
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find nearest commit"
        in
        let nearest_commit_index = nearest_commit_index + my_deposit_index in
        let check_accepted :
            Bridge.Check_accepted_mina.Init.t
            * Bridge.Check_accepted_mina.Elem.t list =
          ( { params = deposit_params
            ; original_action_state =
                C.Rollup_state.Outer_action_state.unsafe_value_of_field
                  before_my_deposit_action_state
            ; deposit_index = UInt32.of_int my_deposit_index
            }
          , List.sub actions ~pos:my_deposit_index
              ~len:(nearest_commit_index - my_deposit_index + 1)
            |> List.map ~f:(fun (action, _, _) -> action) )
        in
        let commit_ase : Ase.Without_length.Stmt.t * Field.t list =
          ( after_nearest_commit_action_state
          , List.sub actions ~pos:(nearest_commit_index + 1)
              ~len:(List.length actions - nearest_commit_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let check_accepted_ase : Ase.With_length.Stmt.t * Field.t list =
          ( { action_state = fst commit_ase
            ; length =
                Zeko_circuits.Zeko_util.Checked32.of_int
                  (nearest_commit_index + 1)
            }
          , snd commit_ase )
        in
        let sync_ase : Ase.With_length.Stmt.t * Field.t list =
          let commit_sync_index =
            ( Zeko_circuits.Rollup_state.Outer_action_state.With_length.length
                nearest_commit.synchronized_outer_action_state
            |> Account_nonce.to_int )
            - 1
          in
          ( { action_state =
                C.Rollup_state.Outer_action_state.(
                  With_length.raw nearest_commit.synchronized_outer_action_state)
            ; length =
                C.Rollup_state.Outer_action_state.(
                  With_length.length
                    nearest_commit.synchronized_outer_action_state)
            }
          , List.sub actions ~pos:(commit_sync_index + 1)
              ~len:(List.length actions - commit_sync_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let%bind prev_next_cancelled_deposit =
          let helper_aid =
            Account_id.create
              (Public_key.compress signer.public_key)
              (Account_id.derive_token_id
                 ~owner:
                   (Account_id.of_public_key
                      (Public_key.decompress_exn
                         Zeko_circuits_config.Inputs.helper_token_owner_l1 ) ) )
          in
          match%map
            Gql_client.fetch_state_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some (next_cancelled_deposit :: _next_withdrawal :: _) ->
              Some (UInt32.of_string (Field.to_string next_cancelled_deposit))
          | None ->
              None
        in
        let%map transfer_forest =
          Bridge_prover.(
            prove !sequencer.bridge_prover
              (Finalize_cancelled_deposit.f ~logger
                 { public_key =
                     List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
                 ; commit = nearest_commit
                 ; before_commit =
                     C.Rollup_state.Outer_action_state.unsafe_value_of_field
                       before_nearest_commit_action_state
                 ; commit_ase_source = fst commit_ase
                 ; commit_ase_elems = snd commit_ase
                 ; sync_ase_source = fst sync_ase
                 ; sync_ase_elems = snd sync_ase
                 ; check_accepted_init = fst check_accepted
                 ; check_accepted_elems = snd check_accepted
                 ; check_accepted_ase_source = fst check_accepted_ase
                 ; check_accepted_ase_elems = snd check_accepted_ase
                 ; prev_next_cancelled_deposit =
                     Option.value prev_next_cancelled_deposit
                       ~default:UInt32.zero
                 } ))
          >>| Or_error.ok_exn
        in
        let transferrer_update =
          Account_update.with_no_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress signer.public_key
              ; balance_change =
                  (let account_creation_fee =
                     if Option.is_some prev_next_cancelled_deposit then
                       Currency.Amount.zero
                     else
                       constraint_constants.account_creation_fee
                       |> Currency.Amount.of_fee
                   in
                   Currency.Amount.(
                     Signed.of_unsigned
                     @@ Option.value_exn
                          ~message:"Amount insufficient to create 2 accounts"
                     @@ sub deposit_params.amount account_creation_fee) )
              ; implicit_account_creation_fee = false
              ; use_full_commitment = true
              ; authorization_kind = None_given
              }
            ~authorization:Control.Poly.None_given
        in
        let transfer_cmd : Zkapp_command.t =
          { fee_payer
          ; account_updates =
              Zkapp_command.Call_forest.cons
                ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                transferrer_update transfer_forest
              |> Zkapp_command.Call_forest.map
                   ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
              |> Utils.rehash_forest
                   ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ; memo = Signed_command_memo.empty
          }
        in
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 transfer_cmd
          [ signer ]
        |> Zkapp_command.read_all_proofs_from_disk
      in
      print_endline "(* Cancel timeouted deposits 7-9 *)" ;
      run (fun () ->
          let%bind () =
            Deferred.List.iteri timeout_deposits
              ~f:(fun i (signer, deposit_params) ->
                printf "(* Canceling deposit %d *)\n%!" i ;
                let%bind command =
                  cancel_deposit ~fee:1 signer deposit_params
                in
                let%bind _ = Gql_client.send_zkapp gql_uri command in
                let%bind _created =
                  Gql_client.For_tests.create_new_block ~logger gql_uri
                in
                let%map status =
                  Gql_client.For_tests.get_zkapp_command_status ~logger gql_uri
                    (Mina_transaction.Transaction_hash.hash_command
                       (Zkapp_command command) )
                in
                [%test_eq: string list list option] status None )
          in
          return () ) ;

      print_endline "Started test 'withdrawals'" ;

      let submit_withdrawal ~fee (signer : Keypair.t)
          (withdrawal_params : C.Bridge_state.Withdrawal_params_base.t) =
        let nonce =
          Sequencer.infer_nonce !sequencer
            (Signature_lib.Public_key.compress signer.public_key)
        in
        let fee_payer =
          Account_update.Fee_payer.
            { body =
                { public_key = Public_key.compress signer.public_key
                ; fee = Currency.Fee.of_mina_int_exn fee
                ; valid_until = None
                ; nonce = Account.Nonce.of_uint32 nonce
                }
            ; authorization = Signature.dummy
            }
        in
        let bridge_proof_fee = Zeko_circuits_config.Inputs.bridge_proof_fee in
        let transferrer_update =
          Account_update.with_no_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress signer.public_key
              ; balance_change =
                  Currency.Amount.Signed.(
                    negate @@ of_unsigned
                    @@ Option.value_exn
                         (Currency.Amount.add withdrawal_params.amount
                            bridge_proof_fee ))
              ; use_full_commitment = false
              ; authorization_kind = Signature
              ; increment_nonce = true
              ; preconditions =
                  { Account_update.Preconditions.accept with
                    account =
                      Zkapp_precondition.Account.nonce
                        (Account.Nonce.succ nonce)
                  }
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        in
        let%map transfer_forest =
          Bridge_prover.(
            prove !sequencer.bridge_prover
              (Withdrawal_request.f ~logger
                 { withdrawal_params; transferrer = transferrer_update } ))
          >>| Or_error.ok_exn
        in
        let transfer_cmd : Zkapp_command.t =
          { fee_payer
          ; account_updates =
              transfer_forest
              |> Zkapp_command.Call_forest.map
                   ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
              |> Utils.rehash_forest
                   ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
          ; memo = Signed_command_memo.empty
          }
        in
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 transfer_cmd
          [ signer ]
      in
      let withdrawal ~amount ~(account : Keypair.t) :
          C.Bridge_state.Withdrawal_params_base.t =
        { children = []
        ; recipient = Public_key.compress account.public_key
        ; amount = Currency.Amount.of_mina_int_exn amount
        }
      in

      let account1 = List.nth_exn l1_accounts 0 in
      let account2 = List.nth_exn l1_accounts 1 in
      let account3 = List.nth_exn l1_accounts 2 in

      print_endline "(* Send 1-3 withdrawals *)" ;
      let withdrawals =
        run (fun () ->
            let withdrawal1 = withdrawal ~amount:5 ~account:account1 in
            let withdrawal2 = withdrawal ~amount:15 ~account:account2 in
            let withdrawal3 = withdrawal ~amount:25 ~account:account3 in

            let%bind () =
              submit_withdrawal ~fee:6 account1 withdrawal1
              >>= fun command ->
              apply_user_command !sequencer (Zkapp_command command)
              >>| [%test_eq: unit Or_error.t] (Ok ())
            in
            let%bind () =
              submit_withdrawal ~fee:5 account2 withdrawal2
              >>= fun command ->
              apply_user_command !sequencer (Zkapp_command command)
              >>| [%test_eq: unit Or_error.t] (Ok ())
            in
            let%bind () =
              submit_withdrawal ~fee:4 account3 withdrawal3
              >>= fun command ->
              apply_user_command !sequencer (Zkapp_command command)
              >>| [%test_eq: unit Or_error.t] (Ok ())
            in
            return
              [ (account1, withdrawal1)
              ; (account2, withdrawal2)
              ; (account3, withdrawal3)
              ] )
      in

      print_endline "(* Commit 1-3 withdrawals *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Send 4-6 withdrawals *)" ;
      let withdrawals =
        withdrawals
        @ run (fun () ->
              let withdrawal4 = withdrawal ~amount:35 ~account:account1 in
              let withdrawal5 = withdrawal ~amount:45 ~account:account2 in
              let withdrawal6 = withdrawal ~amount:55 ~account:account3 in

              let%bind () =
                submit_withdrawal ~fee:3 account1 withdrawal4
                >>= fun command ->
                apply_user_command !sequencer (Zkapp_command command)
                >>| [%test_eq: unit Or_error.t] (Ok ())
              in
              let%bind () =
                submit_withdrawal ~fee:2 account2 withdrawal5
                >>= fun command ->
                apply_user_command !sequencer (Zkapp_command command)
                >>| [%test_eq: unit Or_error.t] (Ok ())
              in
              let%bind () =
                submit_withdrawal ~fee:1 account3 withdrawal6
                >>= fun command ->
                apply_user_command !sequencer (Zkapp_command command)
                >>| [%test_eq: unit Or_error.t] (Ok ())
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block ~logger gql_uri
              in
              return
                [ (account1, withdrawal4)
                ; (account2, withdrawal5)
                ; (account3, withdrawal6)
                ] )
      in

      print_endline "(* Commit 4-6 withdrawals *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let finalize_withdrawal ~fee (signer : Keypair.t) withdrawal_params =
        let%bind nonce =
          Gql_client.fetch_nonce ~logger gql_uri
            (Signature_lib.Public_key.compress signer.public_key)
          >>| Or_error.ok_exn
        in
        let fee_payer =
          Account_update.Fee_payer.
            { body =
                { public_key = Public_key.compress signer.public_key
                ; fee = Currency.Fee.of_mina_int_exn fee
                ; valid_until = None
                ; nonce = Account.Nonce.of_uint32 nonce
                }
            ; authorization = Signature.dummy
            }
        in
        let%bind l1_actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( last_commit_index
            , (last_commit, `Before before_last_commit, `After after_last_commit)
            ) =
          List.rev l1_actions
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find any commit"
        in
        let last_commit_index =
          List.length l1_actions - last_commit_index - 1
        in
        let commit_ase =
          ( after_last_commit
          , List.sub l1_actions ~pos:(last_commit_index + 1)
              ~len:(List.length l1_actions - last_commit_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let before_withdrawal, withdrawal_ase =
          let actions =
            Archive.get_actions !sequencer.archive
              Zeko_constants.inner_account_id ~from:None
              ~to_:
                (Some
                   C.Rollup_state.Inner_action_state.(
                     With_length.state last_commit.inner_action_state |> raw) )
            |> Result.map_error ~f:Error.of_string
            |> Or_error.ok_exn
            |> List.map ~f:(fun { actions; action_state; _ } ->
                   let (action_state_one :: action_state_two :: _) =
                     action_state
                   in
                   ( Utils.actions_to_inner_action (List.hd_exn actions)
                   , `Before action_state_two
                   , `After action_state_one ) )
          in
          let hashed_withdrawal =
            Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
              C.Bridge_state.Withdrawal_params_base.typ withdrawal_params
          in
          let ( my_withdrawal_index
              , (_, `Before before_withdrawal, `After after_withdrawal) ) =
            List.findi actions ~f:(fun _ (action, _, _) ->
                Field.equal hashed_withdrawal action.aux )
            |> Option.value_exn ~message:"Did not find my withdrawal"
          in
          let elems =
            List.sub actions ~pos:(my_withdrawal_index + 1)
              ~len:(List.length actions - my_withdrawal_index - 1)
            |> List.map ~f:(fun (action, `Before _, `After _) ->
                   [ Utils.actions_of_inner_action_without_forest action ]
                   |> Zkapp_account.Actions_impl.hash )
          in
          ( C.Rollup_state.Inner_action_state.unsafe_value_of_field
              before_withdrawal
          , ( Ase.With_length.Stmt.
                { action_state = after_withdrawal
                ; length =
                    (let committed_length =
                       C.Zeko_util.Checked32.to_int
                         (C.Rollup_state.Inner_action_state.With_length.length
                            last_commit.inner_action_state )
                     in
                     C.Zeko_util.Checked32.of_int
                       (committed_length - List.length elems) )
                }
            , elems ) )
        in
        let helper_aid =
          Account_id.create
            (Public_key.compress signer.public_key)
            (Account_id.derive_token_id
               ~owner:
                 (Account_id.of_public_key
                    (Public_key.decompress_exn
                       Zeko_circuits_config.Inputs.helper_token_owner_l1 ) ) )
        in
        let%bind prev_next_withdrawal =
          match%map
            Gql_client.fetch_state_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some (_next_cancelled_deposit :: next_withdrawal :: _) ->
              Some (UInt32.of_string (Field.to_string next_withdrawal))
          | None ->
              None
        in
        let%bind prev_nonce =
          match%map
            Gql_client.fetch_nonce_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some nonce ->
              nonce
          | None ->
              UInt32.zero
        in
        let%map transfer_forest =
          Bridge_prover.(
            prove !sequencer.bridge_prover
              (Finalize_withdrawal.f ~logger
                 { public_key =
                     List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
                 ; commit = last_commit
                 ; before_commit =
                     C.Rollup_state.Outer_action_state.unsafe_value_of_field
                       before_last_commit
                 ; commit_ase_source = fst commit_ase
                 ; commit_ase_elems = snd commit_ase
                 ; before_withdrawal
                 ; withdrawal_ase_source = fst withdrawal_ase
                 ; withdrawal_ase_elems = snd withdrawal_ase
                 ; prev_next_withdrawal =
                     Option.value prev_next_withdrawal ~default:UInt32.zero
                 ; withdrawal_params
                 ; prev_nonce
                 ; helper_account_new = Option.is_none prev_next_withdrawal
                 } ))
          >>| Or_error.ok_exn
        in
        let transfer_cmd : Zkapp_command.t =
          { fee_payer
          ; account_updates =
              transfer_forest
              |> Zkapp_command.Call_forest.map
                   ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
              |> Utils.rehash_forest
                   ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ; memo = Signed_command_memo.empty
          }
        in
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 transfer_cmd
          [ signer ]
        |> Zkapp_command.read_all_proofs_from_disk
      in

      print_endline "(* Finalize all withdrawals *)" ;
      run (fun () ->
          let%bind _shifted =
            Gql_client.For_tests.shift_slots ~logger gql_uri 200
          in
          let%bind () =
            Deferred.List.iteri withdrawals
              ~f:(fun i (signer, withdrawal_params) ->
                printf "(* Finalizing withdrawal %d *)\n%!" i ;
                let%bind command =
                  finalize_withdrawal ~fee:1 signer withdrawal_params
                in
                let%bind _ = Gql_client.send_zkapp gql_uri command in
                let%bind _created =
                  Gql_client.For_tests.create_new_block ~logger gql_uri
                in
                let%map status =
                  Gql_client.For_tests.get_zkapp_command_status ~logger gql_uri
                    (Mina_transaction.Transaction_hash.hash_command
                       (Zkapp_command command) )
                in
                [%test_eq: string list list option] status None )
          in
          return () ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  Core.printf "Sequencer tests took %s\n%!"
    (Time.Span.to_string (Time.diff (Time.now ()) start_time))
