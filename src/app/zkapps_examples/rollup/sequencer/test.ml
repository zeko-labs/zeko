[@@@warning "-26-33"]

open Core_kernel
open Async
open Async_kernel
open Mina_base
module L = Mina_ledger.Ledger
open Sequencer_lib.Zeko_sequencer
open Sequencer_lib

let () =
  let number_of_transactions = 1 in
  let zkapp_keypair = Signature_lib.Keypair.create () in
  let gql_uri =
    { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
    ; name = "gql-uri"
    }
  in
  let wait_for_new_block () =
    let rec wait_for_new_block' old_block_height =
      let%bind block_height = Gql_client.fetch_block_height gql_uri in
      match block_height > old_block_height with
      | true ->
          Deferred.unit
      | false ->
          let%bind () = after (Time_ns.Span.of_sec 0.5) in
          wait_for_new_block' old_block_height
    in
    let%bind block_height = Gql_client.fetch_block_height gql_uri in
    wait_for_new_block' block_height
  in

  (* Fetch signer *)
  let signer =
    Thread_safe.block_on_async_exn (fun () ->
        let open Cohttp_async in
        let%bind _, body =
          Client.get
            (Uri.of_string
               "http://localhost:8181/acquire-account?unlockAccount=true" )
        in
        let%bind json =
          Deferred.map ~f:Yojson.Safe.from_string (Body.to_string body)
        in
        let sk = Yojson.Safe.Util.(member "sk" json |> to_string) in
        let signer =
          Signature_lib.(
            Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk)
        in
        return signer )
  in

  let open Mina_transaction_logic.For_tests in
  Quickcheck.test ~trials:1
    (Test_spec.mk_gen ~num_transactions:number_of_transactions ())
    ~f:(fun { init_ledger; specs } ->
      let sequencer =
        Sequencer.create
          ~zkapp_pk:Signature_lib.Public_key.(compress zkapp_keypair.public_key)
          ~max_pool_size:10 ~commitment_period_sec:0. ~da_contract_address:None
          ~db_dir:None ~l1_uri:(Some gql_uri) ~signer
      in
      L.with_ledger ~depth:constraint_constants.ledger_depth
        ~f:(fun expected_ledger ->
          (* Init ledgers *)
          L.create_new_account_exn expected_ledger M.Inner.account_id
            M.Inner.initial_account ;
          ( match
              L.Db.get_or_create_account sequencer.db M.Inner.account_id
                M.Inner.initial_account
            with
          | Error _ ->
              failwith "zeko: Failed to create account in sequencer"
          | _ ->
              () ) ;
          Array.iter init_ledger ~f:(fun (keypair, balance) ->
              let pk = Signature_lib.Public_key.compress keypair.public_key in
              let account_id = Account_id.create pk Token_id.default in
              let balance = Unsigned.UInt64.of_int64 balance in
              let account =
                Account.create account_id (Currency.Balance.of_uint64 balance)
              in
              L.create_new_account_exn expected_ledger account_id account ;
              add_account sequencer pk Token_id.default balance ) ;
          let deploy_update = M.Outer.deploy_exn expected_ledger in

          let source_ledger_hash = get_root sequencer in

          [%test_eq: Frozen_ledger_hash.t] source_ledger_hash
            (L.merkle_root expected_ledger) ;

          (* Deploy *)
          Thread_safe.block_on_async_exn (fun () ->
              ( print_endline
              @@ Signature_lib.Public_key.(
                   Compressed.to_base58_check
                   @@ compress zkapp_keypair.public_key) ) ;
              let%bind nonce =
                Gql_client.fetch_nonce gql_uri
                  (Signature_lib.Public_key.compress signer.public_key)
              in
              let command =
                Deploy.deploy deploy_update ~fee_payer:signer
                  ~fee_payer_nonce:(Account.Nonce.of_int nonce)
                  ~zkapp_keypair
              in
              let%bind _ = Gql_client.send_zkapp gql_uri command in
              wait_for_new_block () ) ;

          (* Apply commands *)
          let target_ledger_hash =
            Thread_safe.block_on_async_exn (fun () ->
                List.iter specs ~f:(fun spec ->
                    let txn_applied =
                      match
                        Quickcheck.random_value Bool.quickcheck_generator
                      with
                      | true ->
                          let command = account_update_send spec in
                          ( match
                              L.apply_zkapp_command_unchecked expected_ledger
                                command ~constraint_constants
                                ~global_slot:
                                  (Mina_numbers.Global_slot_since_genesis.of_int
                                     sequencer.slot )
                                ~state_view:
                                  (Mina_state.Protocol_state.Body.view
                                     state_body )
                            with
                          | Ok _ ->
                              ()
                          | _ ->
                              () ) ;

                          apply_zkapp_command sequencer command ~with_prove:true
                      | false ->
                          let command = command_send spec in
                          ( match
                              L.apply_user_command_unchecked expected_ledger
                                command ~constraint_constants
                                ~txn_global_slot:
                                  (Mina_numbers.Global_slot_since_genesis.of_int
                                     sequencer.slot )
                            with
                          | Ok _ ->
                              ()
                          | _ ->
                              () ) ;

                          apply_signed_command sequencer command
                            ~with_prove:true
                    in
                    [%test_eq: Bool.t] true (Or_error.is_ok txn_applied) ;
                    let txn_applied, dproof = Or_error.ok_exn txn_applied in

                    don't_wait_for dproof ;

                    let status =
                      L.Transaction_applied.transaction_status txn_applied
                    in
                    [%test_eq: Transaction_status.t] status Applied ) ;

                let target_ledger_hash = get_root sequencer in

                [%test_eq: Frozen_ledger_hash.t] target_ledger_hash
                  (L.merkle_root expected_ledger) ;

                let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in

                [%test_eq: Bool.t] true (Option.is_some sequencer.snark_q.last) ;
                let snark = Option.value_exn sequencer.snark_q.last in
                (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.source_ledger snark)
                  source_ledger_hash ;
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.target_ledger snark)
                  target_ledger_hash ;

                let%bind res = M.Wrapper.verify snark in
                [%test_eq: Bool.t] true (Or_error.is_ok res) ;

                return target_ledger_hash )
          in

          (* Commit *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind () = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () = wait_for_new_block () in
              let%bind commited_ledger_hash =
                Gql_client.fetch_commited_state gql_uri
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              in
              [%test_eq: Frozen_ledger_hash.t] commited_ledger_hash
                target_ledger_hash ;

              Deferred.unit ) ) )
