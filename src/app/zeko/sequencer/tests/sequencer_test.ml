open Core_kernel
open Async
open Mina_base
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Sequencer
open Zeko_types

let constraint_constants = Zeko_constants.constraint_constants

let start_time = Time.now ()

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Debug ;
  Logger.create ()

let number_of_transactions = 5

let gql_uri =
  { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
  ; name = "gql-uri"
  }

let da_config = Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555" ]

let provers =
  [ Host_and_port.create ~host:"localhost" ~port:9990
  ; Host_and_port.create ~host:"localhost" ~port:9991
  ]

let l1_network_id = "testnet"

let l1_signature_kind = Utils.signature_kind l1_network_id

let l2_network_id = "testnet"

let l2_signature_kind = Utils.signature_kind l2_network_id

let run = Thread_safe.block_on_async_exn

module Sequencer_test_spec = struct
  type t =
    { zkapp_keypair : Keypair.t
    ; signer : Keypair.t
    ; ephemeral_ledger : L.t (* The ledger to test the expected outcome *)
    ; specs : Mina_transaction_logic.For_tests.Transaction_spec.t list
          (* Transaction specs *)
    ; sequencer : Sequencer.t
    }

  let gen ?(delay_deposit = 0) ?db_dir () =
    let zkapp_keypair = Keypair.create () in

    print_endline "(* Create signer *)" ;
    let rec create_even_signer () =
      let signer = Keypair.create () in
      let compressed = Public_key.compress signer.public_key in
      if compressed.is_odd then create_even_signer () else signer
    in
    let signer = create_even_signer () in
    run (fun () ->
        let%bind _res =
          Gql_client.For_tests.create_account gql_uri
            (Public_key.compress signer.public_key)
        in
        return () ) ;

    let%bind.Quickcheck.Generator { init_ledger; specs } =
      Mina_transaction_logic.For_tests.Test_spec.mk_gen
        ~num_transactions:number_of_transactions ()
    in

    let initial_inner_account = run Deploy.Z.Inner.initial_account in
    let genesis_accounts =
      (Zeko_constants.inner_account_id, initial_inner_account)
      :: ( Array.map init_ledger ~f:(fun (keypair, balance) ->
               let pk = Signature_lib.Public_key.compress keypair.public_key in
               let account_id = Account_id.create pk Token_id.default in
               let balance = Unsigned.UInt64.of_int64 balance in
               let account =
                 Account.create account_id (Currency.Balance.of_uint64 balance)
               in
               (account_id, account) )
         |> Array.to_list )
    in

    print_endline "(* Init ephemeral ledger *)" ;
    let ephemeral_ledger =
      L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
    in
    List.iter genesis_accounts ~f:(fun (aid, acc) ->
        L.create_new_account_exn ephemeral_ledger aid acc ) ;
    let account_set_hash =
      let db =
        Indexed_merkle_tree.Db.create ~depth:constraint_constants.ledger_depth
          ()
      in
      let tids =
        List.map genesis_accounts ~f:(fun (aid, _) ->
            Account_id.derive_token_id ~owner:aid )
      in
      List.iter tids ~f:(fun tid ->
          let _, _ = Indexed_merkle_tree.Db.get_or_create_entry_exn db tid in
          () ) ;
      Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root db |]
    in

    print_endline "(* Post genesis batch *)" ;
    run (fun () ->
        match%bind
          Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
            ~ledger:ephemeral_ledger
        with
        | Ok _ ->
            return ()
        | Error e ->
            Error.raise e ) ;

    print_endline "(* Deploy zkapp *)" ;
    run (fun () ->
        let sequencer_pk =
          Public_key.compress signer.public_key |> Even_PC.create_exn
        in
        let%bind da_key =
          Da_layer.Client.Rpc.get_node_public_key ~logger
            ~node_location:(List.hd_exn da_config.nodes)
            ()
          >>| Or_error.ok_exn >>| Even_PC.create_exn
        in
        ( print_endline
        @@ Public_key.(
             Compressed.to_base58_check @@ compress zkapp_keypair.public_key) ) ;
        let%bind nonce =
          Gql_client.infer_nonce gql_uri (Public_key.compress signer.public_key)
        in
        let%bind command =
          Deploy.deploy_command_exn ~signature_kind:l1_signature_kind ~signer
            ~zkapp:zkapp_keypair
            ~fee:(Currency.Fee.of_mina_int_exn 1)
            ~nonce ~initial_ledger:ephemeral_ledger
            ~account_creation_fee:constraint_constants.account_creation_fee
            ~account_set_hash ~pause_key:sequencer_pk ~sequencer:sequencer_pk
            ~da_key ()
        in
        let%bind _ = Gql_client.send_zkapp gql_uri command in
        let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
        return () ) ;

    print_endline "(* Init sequencer *)" ;
    let sequencer =
      run (fun () ->
          Sequencer.create ~logger
            ~zkapp_pk:
              Signature_lib.Public_key.(compress zkapp_keypair.public_key)
            ~max_pool_size:10 ~commitment_period_sec:0. ~da_config ~da_quorum:1
            ~db_dir ~l1_uri:gql_uri ~archive_uri:gql_uri ~signer ~l1_network_id
            ~l2_network_id ~deposit_delay_blocks:delay_deposit ~provers )
    in

    Quickcheck.Generator.return
      { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer }
end

let () =
  print_endline "Started test 'apply commands and commit'" ;
  Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ())
    ~f:(fun { zkapp_keypair; signer; specs; sequencer; _ } ->
      let batch1, batch2 = List.split_n specs 3 in

      print_endline "(* Apply first batch *)" ;
      let () =
        run (fun () ->
            let%bind () =
              Deferred.List.iteri batch1 ~f:(fun i spec ->
                  let%bind result =
                    match i % 2 = 0 with
                    | true ->
                        let command =
                          Mina_transaction_logic.For_tests.account_update_send
                            ~chain:l2_signature_kind spec
                        in
                        printf "Applying zkapp command\n%!" ;
                        apply_user_command sequencer (Zkapp_command command)
                    | false ->
                        let command =
                          Mina_transaction_logic.For_tests.command_send
                            ~chain:l2_signature_kind spec
                        in
                        printf "Applying signed command\n%!" ;
                        apply_user_command sequencer (Signed_command command)
                  in
                  let witnesses =
                    match result with
                    | Ok result ->
                        result
                    | Error e ->
                        Error.raise e
                  in
                  match%map
                    Deferred.List.map ~how:`Sequential witnesses
                      ~f:(fun witness ->
                        Merger.P.add_job sequencer.db_pool sequencer.merger
                          sequencer.merger_ctx ~data:witness )
                    >>| Result.all
                    >>| Result.map ~f:(fun x -> List.iter x ~f:Fn.id)
                  with
                  | Ok () ->
                      ()
                  | Error e ->
                      failwith (Caqti_error.show e) )
            in
            return () )
      in

      print_endline "(* First commit *)" ;
      run (fun () ->
          let%bind () = commit sequencer in
          let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
          let%bind () = Executor.wait_to_finish sequencer.merger_ctx.executor in
          let%bind { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state gql_uri
              ~signer_pk:(Public_key.compress signer.public_key)
              ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

          Deferred.unit ) ;

      (* To test nonce inferring from pool *)
      (* The first commit is still in the pool *)
      Executor.refresh_nonce sequencer.merger_ctx.executor ;

      print_endline "(* Apply second batch *)" ;
      run (fun () ->
          let%bind () =
            Deferred.List.iteri batch2 ~f:(fun i spec ->
                let%bind result =
                  match i % 2 = 0 with
                  | true ->
                      let command =
                        Mina_transaction_logic.For_tests.account_update_send
                          ~chain:l2_signature_kind spec
                      in
                      printf "Applying zkapp command\n%!" ;
                      apply_user_command sequencer (Zkapp_command command)
                  | false ->
                      let command =
                        Mina_transaction_logic.For_tests.command_send
                          ~chain:l2_signature_kind spec
                      in
                      printf "Applying signed command\n%!" ;
                      apply_user_command sequencer (Signed_command command)
                in

                let witnesses =
                  match result with
                  | Ok result ->
                      result
                  | Error e ->
                      Error.raise e
                in

                match%map
                  Deferred.List.map ~how:`Sequential witnesses
                    ~f:(fun witness ->
                      Merger.P.add_job sequencer.db_pool sequencer.merger
                        sequencer.merger_ctx ~data:witness )
                  >>| Result.all
                  >>| Result.map ~f:(fun x -> List.iter x ~f:Fn.id)
                with
                | Ok () ->
                    ()
                | Error e ->
                    failwith (Caqti_error.show e) )
          in
          return () ) ;

      print_endline "(* Second commit *)" ;
      let final_ledger_hash =
        run (fun () ->
            let%bind () = commit sequencer in
            let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
            let%bind () =
              Executor.wait_to_finish sequencer.merger_ctx.executor
            in
            let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
            let%bind { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state gql_uri
                ~signer_pk:(Public_key.compress signer.public_key)
                ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

            return target_ledger_hash )
      in

      print_endline "(* Try to bootstrap again *)" ;
      run (fun () ->
          let%bind new_sequencer =
            Sequencer.create ~logger
              ~zkapp_pk:
                Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              ~max_pool_size:10 ~commitment_period_sec:0. ~da_config
              ~da_quorum:1 ~db_dir:None ~l1_uri:gql_uri ~archive_uri:gql_uri
              ~signer ~l1_network_id ~l2_network_id ~deposit_delay_blocks:0
              ~provers
          in
          return
          @@ [%test_eq: Frozen_ledger_hash.t] (get_root new_sequencer)
               final_ledger_hash ) )

let () =
  print_endline "Started test 'dummy signature should fail'" ;
  Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ())
    ~f:(fun { specs; sequencer; _ } ->
      let dummy_signature_command : Zkapp_command.t =
        let command =
          Mina_transaction_logic.For_tests.account_update_send
            (List.hd_exn specs)
        in
        { command with
          account_updates =
            Zkapp_command.Call_forest.map command.account_updates
              ~f:(fun account_update ->
                match Account_update.authorization account_update with
                | Signature _ ->
                    { account_update with
                      authorization = Signature Signature.dummy
                    }
                | _ ->
                    account_update )
        }
      in
      let result =
        run (fun () ->
            apply_user_command sequencer (Zkapp_command dummy_signature_command) )
      in
      match result with
      | Error e
        when String.is_substring ~substring:"Invalid_signature"
               (Error.to_string_hum e) ->
          ()
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
  Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ~db_dir ())
    ~f:(fun { zkapp_keypair; signer; specs; sequencer; _ } ->
      let () =
        run (fun () ->
            let%bind () =
              Deferred.List.iteri specs ~f:(fun i spec ->
                  let%bind result =
                    match i % 2 = 0 with
                    | true ->
                        let command =
                          Mina_transaction_logic.For_tests.account_update_send
                            ~chain:l2_signature_kind spec
                        in
                        printf "Applying zkapp command\n%!" ;
                        apply_user_command sequencer (Zkapp_command command)
                    | false ->
                        let command =
                          Mina_transaction_logic.For_tests.command_send
                            ~chain:l2_signature_kind spec
                        in
                        printf "Applying signed command\n%!" ;
                        apply_user_command sequencer (Signed_command command)
                  in
                  let witnesses =
                    match result with
                    | Ok result ->
                        result
                    | Error e ->
                        Error.raise e
                  in
                  match%map
                    Deferred.List.map ~how:`Sequential witnesses
                      ~f:(fun witness ->
                        Merger.P.add_job sequencer.db_pool sequencer.merger
                          sequencer.merger_ctx ~data:witness )
                    >>| Result.all
                    >>| Result.map ~f:(fun x -> List.iter x ~f:Fn.id)
                  with
                  | Ok () ->
                      ()
                  | Error e ->
                      failwith (Caqti_error.show e) )
            in
            return () )
      in

      print_endline "(* Restart sequencer *)" ;
      let new_sequencer =
        run (fun () ->
            let%bind () = Sequencer.shutdown sequencer in
            Sequencer.create ~logger
              ~zkapp_pk:
                Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              ~max_pool_size:10 ~commitment_period_sec:0. ~da_config
              ~da_quorum:1 ~db_dir:(Some db_dir) ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer ~l1_network_id ~l2_network_id
              ~deposit_delay_blocks:0 ~provers )
      in

      print_endline "(* Requeue witnesses and commit *)" ;
      run (fun () ->
          let%bind () = commit new_sequencer in
          let%bind () = Snark_queue.wait_to_finish new_sequencer.snark_q in
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
              (fun conn -> Merger.P.Witness_row.get_all conn ())
              new_sequencer.db_pool
            >>| Relational_db.caqti_ok_exn
                  ~msg:"Failed to get all witnesses: %s"
          in
          [%test_eq: int] (List.length all_witnesses) 0 ) )

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
