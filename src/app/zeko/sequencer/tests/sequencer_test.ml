open Core_kernel
open Async
open Mina_base
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Sequencer
open Test_spec
open Handle.Operator

let constraint_constants = Zeko_constants.constraint_constants

let start_time = Time.now ()

let slot_acceptance = Time.Span.of_min 60.

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Debug ;
  Logger.create ()

let gql_uri =
  { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
  ; name = "gql-uri"
  }

let da_config =
  Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555"; "127.0.0.1:8556" ]

let provers =
  [ Host_and_port.create ~host:"localhost" ~port:9990
  ; Host_and_port.create ~host:"localhost" ~port:9991
  ]

let run = Thread_safe.block_on_async_exn

let free_sequencer (sequencer : Sequencer.t Handle.valid_t) =
  Gc.full_major () ;
  run (fun () -> Sequencer.shutdown !sequencer) ;
  Handle.invalidate sequencer

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
       ~postgres_uri:postgres_uri1 ~gql_uri ~da_config ~provers ~slot_acceptance
       () )
    ~f:(fun { zkapp_keypair; signer; specs; sequencer; da_key; accounts; _ } ->
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
          let%bind commit_result = commit !sequencer in
          let%bind _txn_snark = commit_result in
          let%bind () =
            Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
          in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
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
          let%bind commit_result = commit !sequencer in
          let%bind _txn_snark = commit_result in
          let%bind () =
            Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
          in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
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
            let%bind commit_result = commit !sequencer in
            let%bind _txn_snark = commit_result in
            let%bind () =
              Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
            in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
            let%bind { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state gql_uri
                ~signer_pk:(Public_key.compress signer.public_key)
                ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
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
              Sequencer.create ~logger
                ~zkapp_pk:
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
                ~max_pool_size:10 ~commitment_period_sec:0. ~da_config
                ~da_quorum:2 ~db_dir:None ~postgres_uri:postgres_uri2
                ~l1_uri:gql_uri ~archive_uri:gql_uri ~signer
                ~deposit_delay_blocks:0 ~provers ~da_key ~fee_modifier:1.0
                ~minimum_fee:0.01 ~slot_acceptance
                ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
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
    (Sequencer_spec.gen ~logger ~postgres_uri ~gql_uri ~da_config ~provers
       ~slot_acceptance () ) ~f:(fun { specs; sequencer; _ } ->
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
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri ~da_config
       ~provers ~slot_acceptance () )
    ~f:(fun { zkapp_keypair; signer; specs; sequencer; da_key; _ } ->
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
            Sequencer.create ~logger
              ~zkapp_pk:
                Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:
                (Da_layer.Client.Config.of_string_list
                   [ "127.0.0.1:8555"; "127.0.0.1:8556"; "127.0.0.1:8557" ] )
              ~da_quorum:3 ~db_dir:(Some db_dir) ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer ~deposit_delay_blocks:0 ~provers
              ~da_key ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ()) )
      in

      print_endline "(* Requeue witnesses and commit with quorum 3 *)" ;
      run (fun () ->
          let%bind commit_result = commit new_sequencer in
          let%bind _txn_snark = commit_result in
          let%bind () =
            Zeko_prover.Client.wait_to_finish
              new_sequencer.bridge_prover.provers
          in
          let%bind () =
            Executor.wait_to_finish new_sequencer.merger_ctx.executor
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root new_sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

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
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri ~da_config
       ~provers ~slot_acceptance () )
    ~f:(fun { zkapp_keypair; signer; specs; sequencer; da_key; _ } ->
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
          let%bind commit_result = commit !sequencer in
          let%bind _txn_snark = commit_result in
          let%bind () =
            Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
          in
          Executor.wait_to_finish !sequencer.merger_ctx.executor ) ;

      print_endline "(* Apply second batch *)" ;
      run (fun () ->
          Deferred.List.iter batch2 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Second commit *)" ;
      let final_ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer in
            let%bind _txn_snark = commit_result in
            let%bind () =
              Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
            in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _cleared = Gql_client.For_tests.clear_pool gql_uri in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state gql_uri
                ~signer_pk:(Public_key.compress signer.public_key)
                ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
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
            Sequencer.create ~logger
              ~zkapp_pk:
                Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:
                (Da_layer.Client.Config.of_string_list
                   [ "127.0.0.1:8555"; "127.0.0.1:8556" ] )
              ~da_quorum:2 ~db_dir:(Some db_dir) ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer ~deposit_delay_blocks:0 ~provers
              ~da_key ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ()) )
      in

      print_endline "(* Check that after restart it recommited *)" ;
      run (fun () ->
          let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
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
  let genesis_timestamp =
    run (fun () -> Gql_client.fetch_genesis_timestamp gql_uri)
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:5 ~postgres_uri ~gql_uri
       ~da_config ~provers ~slot_acceptance:(Time.Span.of_min 10.) () )
    ~f:(fun { specs; sequencer; signer; zkapp_keypair; _ } ->
      run (fun () ->
          let open Mina_numbers in
          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.l1_global_slot ~genesis_timestamp
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
            Utils.l1_global_slot ~genesis_timestamp
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
            Utils.l1_global_slot ~genesis_timestamp
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
            Utils.l1_global_slot ~genesis_timestamp
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
          let%bind commit_result = commit !sequencer in
          let%bind _txn_snark = commit_result in
          let%bind () =
            Zeko_prover.Client.wait_to_finish !sequencer.bridge_prover.provers
          in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

(* let () =
   print_endline "Started test 'deposits'" ;
   Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ~delay_deposit:2 ())
     ~f:(fun { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer } ->
       (* Create l1 accounts *)
       let l1_accounts =
         Array.create ~len:5 ()
         |> Array.map ~f:Signature_lib.Keypair.create
         |> Array.to_list
       in
       run (fun () ->
           Deferred.List.iter l1_accounts ~f:(fun keypair ->
               let%bind _res =
                 Gql_client.For_tests.create_account gql_uri
                   (Signature_lib.Public_key.compress keypair.public_key)
               in
               return () ) ) ;

       (* Send deposits *)
       let deposits =
         run (fun () ->
             let submit_deposit ~fee (signer : Keypair.t) deposit =
               let%bind nonce =
                 Gql_client.fetch_nonce gql_uri
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
               let%bind transfer_update =
                 M.Outer.submit_deposit
                   ~outer_public_key:
                     (Public_key.compress zkapp_keypair.public_key)
                   ~deposit
               in
               let transferrer_update : Account_update.t =
                 { body =
                     { Account_update.Body.dummy with
                       public_key = Public_key.compress signer.public_key
                     ; balance_change =
                         Currency.Amount.Signed.(
                           negate @@ of_unsigned deposit.amount)
                     ; use_full_commitment = true
                     ; authorization_kind = Signature
                     }
                 ; authorization = Signature Signature.dummy
                 }
               in
               let transfer_cmd : Zkapp_command.t =
                 { fee_payer
                 ; account_updates =
                     Zkapp_command.Call_forest.(
                       cons_tree transfer_update @@ accumulate_hashes'
                       @@ of_account_updates
                            ~account_update_depth:(fun _ -> 0)
                            [ transferrer_update ])
                 ; memo = Signed_command_memo.empty
                 }
               in
               return @@ sign_cmd transfer_cmd [ signer ]
             in
             let account1 = List.nth_exn l1_accounts 0 in
             let account2 = List.nth_exn l1_accounts 1 in
             let account3 = List.nth_exn l1_accounts 2 in
             let account4 = List.nth_exn l1_accounts 3 in
             let account5 = List.nth_exn l1_accounts 4 in

             let deposit1 : Zkapps_rollup.TR.t =
               { recipient = Public_key.compress account1.public_key
               ; amount = Currency.Amount.of_mina_int_exn 10
               }
             in
             let deposit2 : Zkapps_rollup.TR.t =
               { recipient = Public_key.compress account2.public_key
               ; amount = Currency.Amount.of_mina_int_exn 20
               }
             in
             let deposit3 : Zkapps_rollup.TR.t =
               { recipient = Public_key.compress account3.public_key
               ; amount = Currency.Amount.of_mina_int_exn 30
               }
             in
             let deposit4 : Zkapps_rollup.TR.t =
               { recipient = Public_key.compress account4.public_key
               ; amount = Currency.Amount.of_mina_int_exn 40
               }
             in
             let deposit5 : Zkapps_rollup.TR.t =
               { recipient = Public_key.compress account5.public_key
               ; amount = Currency.Amount.of_mina_int_exn 50
               }
             in

             (* Send deposits for accounts 1 and 2 *)
             let%bind _ =
               submit_deposit ~fee:5 account1 deposit1
               >>= Gql_client.send_zkapp gql_uri
             in
             let%bind _ =
               submit_deposit ~fee:4 account2 deposit2
               >>= Gql_client.send_zkapp gql_uri
             in

             (* Create 2 new blocks for delay *)
             let%bind _created =
               Gql_client.For_tests.create_new_block gql_uri
             in
             let%bind _created =
               Gql_client.For_tests.create_new_block gql_uri
             in

             (* Send deposits for accounts 3, 4 and 5 which won't be processed *)
             let%bind _ =
               submit_deposit ~fee:3 account3 deposit3
               >>= Gql_client.send_zkapp gql_uri
             in
             let%bind _ =
               submit_deposit ~fee:2 account4 deposit4
               >>= Gql_client.send_zkapp gql_uri
             in
             let%bind _ =
               submit_deposit ~fee:1 account5 deposit5
               >>= Gql_client.send_zkapp gql_uri
             in
             let%bind _created =
               Gql_client.For_tests.create_new_block gql_uri
             in
             return [ deposit1; deposit2; deposit3; deposit4; deposit5 ] )
       in

       (* Commit should process first 2 deposits *)
       run (fun () ->
           let%bind () = commit sequencer in
           let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
           let%bind () =
             Executor.wait_to_finish sequencer.merger_ctx.executor
           in
           let%bind _created =
             Gql_client.For_tests.create_new_block gql_uri
           in
           let%bind committed_ledger_hash =
             Gql_client.fetch_committed_state gql_uri
               Signature_lib.Public_key.(compress zkapp_keypair.public_key)
           in
           let target_ledger_hash = get_root sequencer in
           [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

           return () ) ;

       let deposits_state =
         Utils.get_inner_deposits_state_exn (L.of_database sequencer.db)
       in
       let expected_deposits_state =
         (* Expected should be only first 2 deposits *)
         List.take deposits 2
         |> List.fold ~init:Zkapp_account.Actions.empty_state_element
              ~f:(fun acc transfer ->
                Zkapp_account.Actions.push_events acc
                  (Zkapps_rollup.TR.to_actions transfer) )
       in
       [%test_eq: Field.t] deposits_state expected_deposits_state ;

       print_endline "Processing remaining deposits" ;

       (* Create new blocks to process remaining deposits *)
       run (fun () ->
           let%bind _created =
             Gql_client.For_tests.create_new_block gql_uri
           in
           let%bind _created =
             Gql_client.For_tests.create_new_block gql_uri
           in
           return () ) ;

       (* Commit should process remaining deposits *)
       run (fun () ->
           let%bind () = commit sequencer in
           let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
           let%bind () =
             Executor.wait_to_finish sequencer.merger_ctx.executor
           in
           let%bind _created =
             Gql_client.For_tests.create_new_block gql_uri
           in
           let%bind committed_ledger_hash =
             Gql_client.fetch_committed_state gql_uri
               Signature_lib.Public_key.(compress zkapp_keypair.public_key)
           in
           let target_ledger_hash = get_root sequencer in
           [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

           return () ) ;

       let deposits_state =
         Utils.get_inner_deposits_state_exn (L.of_database sequencer.db)
       in
       let expected_deposits_state =
         List.fold deposits ~init:Zkapp_account.Actions.empty_state_element
           ~f:(fun acc transfer ->
             Zkapp_account.Actions.push_events acc
               (Zkapps_rollup.TR.to_actions transfer) )
       in
       [%test_eq: Field.t] deposits_state expected_deposits_state ) *)
let () =
  Core.printf "Sequencer tests took %s\n%!"
    (Time.Span.to_string (Time.diff (Time.now ()) start_time))
