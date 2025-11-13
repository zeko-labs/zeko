open Core_kernel
open Async
open Sequencer_lib
open Signature_lib
open Cli_lib
open Mina_base
open Mina_ledger
module Field = Snark_params.Tick.Field
module Sequencer = Zeko_sequencer.Sequencer

let take2 (a, b, _) = (a, b)

let generate_even_key =
  ( "generate-even-key"
  , Command.basic ~summary:"Generate a private key with an even public key"
      (Command_unix.Param.return (fun () ->
           let keypair = Zeko_types.Even_PC.generate_even_signer () in
           Core.printf "Private key: %s\n"
             (Private_key.to_base58_check keypair.private_key) ;
           Core.printf "Public key: %s\n"
             ( Public_key.compress keypair.public_key
             |> Public_key.Compressed.to_base58_check ) ) ) )

let generate_circuits_config =
  ( "generate-circuits-config"
  , Command.basic ~summary:"Generate a circuits config"
      (Command_unix.Param.return (fun () ->
           let generate_keypair () =
             let kp = Keypair.create () in
             (Public_key.compress kp.public_key, kp.private_key)
           in
           let holder_accounts_l1 = [ generate_keypair () ] in
           let helper_token_owner_l1 = generate_keypair () in
           let zeko_l1 = generate_keypair () in
           let t : Zeko_circuits_config.t =
             { chain_l1 = Testnet
             ; chain_l2 = Testnet
             ; max_valid_while_size = Zeko_circuits.Zeko_util.Slot.max_value
             ; holder_accounts_l1 = List.map holder_accounts_l1 ~f:fst
             ; helper_token_owner_l1 = fst helper_token_owner_l1
             ; zeko_l1 = fst zeko_l1
             ; withdrawal_delay = Mina_numbers.Global_slot_span.of_int 5
             }
           in
           let deploy_config : Zeko_circuits_config.Deploy.t =
             { holder_accounts_l1 = List.map holder_accounts_l1 ~f:snd
             ; helper_token_owner_l1 = snd helper_token_owner_l1
             ; zeko_l1 = snd zeko_l1
             }
           in
           Core.printf "circuits config: %s\n%!"
             (Yojson.Safe.pretty_to_string @@ Zeko_circuits_config.to_yojson t) ;
           Core.printf "deploy config: %s\n%!"
             ( Yojson.Safe.pretty_to_string
             @@ Zeko_circuits_config.Deploy.to_yojson deploy_config ) ) ) )

let update_outer_verification_keys =
  ( "update-outer-verification-keys"
  , Command.async ~summary:"Update the verification keys of the outer zkApps"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let open Zeko_types in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         let pp label (real_vk, old_vk) =
           let hash = Compile_simple.Verification_key.hash real_vk in
           [%log info]
             !"%s vk:\n\
               curr: %{sexp: Field.t}\n\
               real: %{sexp: Field.t}\n\
               equal: %b"
             label hash old_vk (Field.equal hash old_vk)
         in

         let%bind fetched_outer_vk =
           Gql_client.fetch_vk l1_uri
             ( Account_id.of_public_key
             @@ Public_key.decompress_exn Zeko_circuits_config.t.zeko_l1 )
           >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         and fetched_bridge_holder_vk =
           Gql_client.fetch_vk l1_uri
             ( Account_id.of_public_key @@ Public_key.decompress_exn
             @@ List.hd_exn Zeko_circuits_config.t.holder_accounts_l1 )
           >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         and fetched_helper_token_owner_vk =
           Gql_client.fetch_vk l1_uri
             ( Account_id.of_public_key
             @@ Public_key.decompress_exn
                  Zeko_circuits_config.t.helper_token_owner_l1 )
           >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         in
         let%bind outer_vk =
           Lazy.force Outer_rules_inst.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         and bridge_holder_vk =
           Lazy.force Bridge_inst_mina.System_L1_enabled.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         and helper_token_owner_vk =
           Lazy.force Bridge_inst_mina.System_L1_token_owner.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         in
         let deploy_config =
           Option.value_exn Zeko_circuits_config.deploy_config
         in
         let outer =
           ( outer_vk
           , fetched_outer_vk
           , Keypair.of_private_key_exn deploy_config.zeko_l1 )
         in
         let bridge_holders =
           List.map deploy_config.holder_accounts_l1 ~f:(fun sk ->
               ( bridge_holder_vk
               , fetched_bridge_holder_vk
               , Keypair.of_private_key_exn sk ) )
         in
         let helper_token_owner =
           ( helper_token_owner_vk
           , fetched_helper_token_owner_vk
           , Keypair.of_private_key_exn deploy_config.helper_token_owner_l1 )
         in
         pp "Core rollup" (take2 outer) ;
         pp "Bridge holder" (take2 @@ List.hd_exn bridge_holders) ;
         pp "Helper token owner" (take2 helper_token_owner) ;

         if only_check then return ()
         else
           let%bind nonce =
             Gql_client.infer_nonce l1_uri
               (Public_key.compress sender.public_key)
           in
           let to_update =
             List.filter_map
               ([ outer; helper_token_owner ] @ bridge_holders)
               ~f:(fun (new_vk, old_vk, kp) ->
                 let hash = Compile_simple.Verification_key.hash new_vk in
                 if Field.equal hash old_vk then None else Some (new_vk, kp) )
           in
           let command =
             Deploy.update_verification_keys
               ~signature_kind:Zeko_circuits_config.t.chain_l1 ~signer:sender
               ~fee:(Currency.Fee.of_mina_string_exn "0.1")
               ~nonce to_update
             |> Zkapp_command.read_all_proofs_from_disk
           in
           match%map Gql_client.send_zkapp l1_uri command with
           | Ok _ ->
               let txn_hash =
                 Mina_transaction.Transaction_hash.hash_command
                   (Zkapp_command command)
               in
               [%log info] "Successfully sent zkapp command: %s"
                 (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
           | Error (`Failed_request err) ->
               [%log error] "Failed request: %s" err
           | Error (`Graphql_error err) ->
               [%log error] "Graphql request: %s" err ) )

let update_inner_verification_keys =
  ( "update-inner-verification-keys"
  , Command.async ~summary:"Update the verification keys of the inner zkApps"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and da_node = flag "--da-node" (required string) ~doc:"string DA node"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let open Zeko_types in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         (* Fetch current state *)
         let%bind commited_ledger_hash =
           Gql_client.infer_state l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Utils.value_of_zkapp_state
                 Zeko_circuits.Rollup_state.Outer_state.typ
           >>| fun { ledger_hash; _ } -> ledger_hash
         in

         (* Sync ledger *)
         let ledger =
           Ledger.create_ephemeral
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         in
         let da_config = Da_layer.Client.Config.of_string_list [ da_node ] in
         let%bind () =
           Da_layer.Client.map_diffs ~logger ~config:da_config
             ~depth:Zeko_constants.constraint_constants.ledger_depth
             ~source_ledger_hash:`Genesis
             ~target_ledger_hash:commited_ledger_hash
             ~f:(fun ~current_chunk ~current_diff:_ ~chunks_length diff ->
               assert (
                 Ledger_hash.equal
                   (Da_layer.Diff.Stable.Latest.source_ledger_hash diff)
                   (Ledger.merkle_root ledger) ) ;
               let progress =
                 Float.of_int current_chunk /. Float.of_int chunks_length
               in
               [%log info] "Sync progress: %.2f%%" (progress *. 100.0) ;
               let changed_accounts =
                 Da_layer.Diff.Stable.Latest.changed_accounts diff
                 |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
               in
               List.iter changed_accounts ~f:(fun (index, account) ->
                   Ledger.set_at_index_exn ledger index account ) ;
               return () )
           >>| Or_error.ok_exn >>| ignore
         in

         let pp label (real_vk, old_vk) =
           let hash = Compile_simple.Verification_key.hash real_vk in
           [%log info]
             !"%s vk:\n\
               curr: %{sexp: Field.t}\n\
               real: %{sexp: Field.t}\n\
               equal: %b"
             label hash old_vk (Field.equal hash old_vk)
         in
         let get_acc ledger pk =
           let aid = Account_id.of_public_key @@ Public_key.decompress_exn pk in
           let%bind.Option loc = Ledger.location_of_account ledger aid in
           let%map.Option acc = Ledger.get ledger loc in
           (acc, Ledger.index_of_account_exn ledger aid)
         in
         let get_vk ledger pk =
           let%bind.Option acc, _ = get_acc ledger pk in
           let%bind.Option zkapp = acc.zkapp in
           zkapp.verification_key
         in

         (* Get vks from synced ledger *)
         let fetched_inner_vk =
           get_vk ledger Zeko_circuits_config.Inputs.inner_public_key
           |> Option.value_exn |> With_hash.hash
         in
         let fetched_bridge_holder_vk =
           get_vk ledger Zeko_circuits_config.Inputs.holder_account_l2
           |> Option.value_exn |> With_hash.hash
         in

         (* Get compiled vks *)
         let%bind inner_vk =
           Lazy.force Inner_rules_inst.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         and bridge_holder_vk =
           Lazy.force Bridge_inst_mina.System_L2.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         in
         (* let deploy_config =
              Option.value_exn Zeko_circuits_config.deploy_config
            in *)
         let inner =
           ( inner_vk
           , fetched_inner_vk
           , Zeko_circuits_config.Inputs.inner_public_key )
         in
         let bridge_holder =
           ( bridge_holder_vk
           , fetched_bridge_holder_vk
           , Zeko_circuits_config.Inputs.holder_account_l2 )
         in
         pp "Core rollup" (take2 inner) ;
         pp "Bridge holder" (take2 bridge_holder) ;

         if only_check then return ()
         else
           (* Find accounts to update *)
           let diff =
             List.filter_map [ inner; bridge_holder ]
               ~f:(fun (new_vk, old_vk, pk) ->
                 let hash = Compile_simple.Verification_key.hash new_vk in
                 if Field.equal hash old_vk then None else Some (new_vk, pk) )
             |> List.map ~f:(fun (new_vk, pk) ->
                    let acc, index = get_acc ledger pk |> Option.value_exn in
                    let zkapp = acc.zkapp |> Option.value_exn in
                    ( index
                    , { acc with
                        zkapp =
                          Some
                            { zkapp with
                              verification_key =
                                Some
                                  (Verification_key_wire.Stable.Latest.M
                                   .of_binable
                                     ( match
                                         Is_compile_simple_real
                                         .is_compile_simple_real
                                       with
                                     | Some eq ->
                                         let _, vk_eq =
                                           Type_equal.detuple2 eq
                                         in
                                         Type_equal.conv vk_eq new_vk
                                     | None ->
                                         Pickles.Side_loaded.Verification_key
                                         .dummy ) )
                            }
                      } ) )
           in

           (* Update the ledegr *)
           let source_ledger_hash = Ledger.merkle_root ledger in
           let ledger_openings =
             Sparse_ledger.of_ledger_subset_exn ledger
               (List.map diff ~f:(fun (_, acc) -> Account.identifier acc))
           in
           List.iter diff ~f:(fun (index, acc) ->
               Ledger.set_at_index_exn ledger index acc ) ;
           let target_ledger_hash = Ledger.merkle_root ledger in

           (* Distribute diff to DA layer *)
           let diff =
             Da_layer.Diff.create ~source_ledger_hash ~changed_accounts:diff
               ~command_with_action_step_flags:None
           in
           let%bind () =
             Da_layer.Client.distribute_diff ~logger ~config:da_config
               ~ledger_openings ~diff
           in
           let%bind command =
             let%map nonce =
               Gql_client.infer_nonce l1_uri
                 (Public_key.compress sender.public_key)
             in
             let open Zeko_circuits.Rollup_state in
             Deploy.update_outer_state
               ~signature_kind:Zeko_circuits_config.t.chain_l1 ~signer:sender
               ~fee:(Currency.Fee.of_mina_string_exn "0.1")
               ~nonce
               ~precondition:
                 Outer_state.
                   { pause_key = None
                   ; paused = None
                   ; ledger_hash =
                       Some (Ledger_hash.var_of_t source_ledger_hash)
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = None
                   ; acc_set = None
                   }
               ~update:
                 Outer_state.
                   { pause_key = None
                   ; paused = None
                   ; ledger_hash =
                       Some (Ledger_hash.var_of_t target_ledger_hash)
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = None
                   ; acc_set = None
                   }
             |> Zkapp_command.read_all_proofs_from_disk
           in
           match%map Gql_client.send_zkapp l1_uri command with
           | Ok _ ->
               let txn_hash =
                 Mina_transaction.Transaction_hash.hash_command
                   (Zkapp_command command)
               in
               [%log info] "Successfully sent zkapp command: %s"
                 (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
           | Error (`Failed_request err) ->
               [%log error] "Failed request: %s" err
           | Error (`Graphql_error err) ->
               [%log error] "Graphql request: %s" err ) )

let update_da_key =
  ( "update-da-key"
  , Command.async ~summary:"Update the DA key of the outer zkApp"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and da_keys =
         flag "--da-key" (listed string) ~doc:"string list of DA keys"
       and da_quorum = flag "--quorum" (required int) ~doc:"int DA quorum"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let open Zeko_types in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         (* Fetch current da key *)
         let%bind current_da_key =
           Gql_client.infer_state l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Utils.value_of_zkapp_state
                 Zeko_circuits.Rollup_state.Outer_state.typ
           >>| fun { da_key; _ } -> da_key
         in
         let new_da_key =
           Multisig.commit
             { public_keys =
                 List.map da_keys ~f:Public_key.Compressed.of_base58_check_exn
             ; quorum = Field.of_int da_quorum
             }
         in

         [%log info]
           !"Current DA key: %{sexp: Field.t}\n\
             New DA key: %{sexp: Field.t}\n\
             equal: %b"
           current_da_key new_da_key
           (Field.equal current_da_key new_da_key) ;

         if only_check then return ()
         else
           let%bind command =
             let%map nonce =
               Gql_client.infer_nonce l1_uri
                 (Public_key.compress sender.public_key)
             in
             let open Zeko_circuits.Rollup_state in
             Deploy.update_outer_state
               ~signature_kind:Zeko_circuits_config.t.chain_l1 ~signer:sender
               ~fee:(Currency.Fee.of_mina_string_exn "0.1")
               ~nonce
               ~precondition:
                 Outer_state.
                   { pause_key = None
                   ; paused = None
                   ; ledger_hash = None
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = Some (Field.Var.constant current_da_key)
                   ; acc_set = None
                   }
               ~update:
                 Outer_state.
                   { pause_key = None
                   ; paused = None
                   ; ledger_hash = None
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = Some (Field.Var.constant new_da_key)
                   ; acc_set = None
                   }
             |> Zkapp_command.read_all_proofs_from_disk
           in
           match%map Gql_client.send_zkapp l1_uri command with
           | Ok _ ->
               let txn_hash =
                 Mina_transaction.Transaction_hash.hash_command
                   (Zkapp_command command)
               in
               [%log info] "Successfully sent zkapp command: %s"
                 (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
           | Error (`Failed_request err) ->
               [%log error] "Failed request: %s" err
           | Error (`Graphql_error err) ->
               [%log error] "Graphql request: %s" err ) )

let update_permissions =
  ( "update-permissions"
  , Command.async ~summary:""
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI" in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         let%bind nonce =
           Gql_client.infer_nonce l1_uri (Public_key.compress sender.public_key)
         in
         let%bind command =
           Deploy.update_permissions
             ~signature_kind:Zeko_circuits_config.t.chain_l1 ~signer:sender
             ~fee:(Currency.Fee.of_mina_string_exn "0.1")
             ~nonce ~gql_uri:l1_uri
             ~permissions:
               { edit_state = Either
               ; send = Proof
               ; receive = None
               ; set_delegate = Proof
               ; set_permissions = Proof
               ; set_verification_key =
                   (Either, Mina_numbers.Txn_version.current)
               ; set_zkapp_uri = Proof
               ; edit_action_state = Proof
               ; set_token_symbol = Proof
               ; increment_nonce = Proof
               ; set_voting_for = Proof
               ; set_timing = Proof
               ; access = None
               }
           >>| Zkapp_command.read_all_proofs_from_disk
         in
         match%map Gql_client.send_zkapp l1_uri command with
         | Ok _ ->
             let txn_hash =
               Mina_transaction.Transaction_hash.hash_command
                 (Zkapp_command command)
             in
             [%log info] "Successfully sent zkapp command: %s"
               (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
         | Error (`Failed_request err) ->
             [%log error] "Failed request: %s" err
         | Error (`Graphql_error err) ->
             [%log error] "Graphql request: %s" err ) )

let set_pause =
  ( "set-pause"
  , Command.async ~summary:"Set the pause of the outer zkapp"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and value =
         flag "--value" (required bool) ~doc:"bool Value to set the pause to"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         (* Fetch current state *)
         let%bind current_paused =
           Gql_client.infer_state l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Utils.value_of_zkapp_state
                 Zeko_circuits.Rollup_state.Outer_state.typ
           >>| fun { paused; _ } -> paused
         in

         [%log info] "Current paused: %b" current_paused ;

         let%bind command =
           let%map nonce =
             Gql_client.infer_nonce l1_uri
               (Public_key.compress sender.public_key)
           in
           let open Zeko_circuits in
           Deploy.update_outer_state
             ~signature_kind:Zeko_circuits_config.t.chain_l1 ~signer:sender
             ~fee:(Currency.Fee.of_mina_string_exn "0.1")
             ~nonce
             ~precondition:
               Rollup_state.Outer_state.
                 { pause_key = None
                 ; paused = None
                 ; ledger_hash = None
                 ; inner_action_state = { state = None; length = None }
                 ; sequencer = None
                 ; da_key = None
                 ; acc_set = None
                 }
             ~update:
               Rollup_state.Outer_state.
                 { pause_key = None
                 ; paused =
                     ( if value then Some Zeko_util.Boolean.true_
                     else Some Zeko_util.Boolean.false_ )
                 ; ledger_hash = None
                 ; inner_action_state = { state = None; length = None }
                 ; sequencer = None
                 ; da_key = None
                 ; acc_set = None
                 }
           |> Zkapp_command.read_all_proofs_from_disk
         in
         match%map Gql_client.send_zkapp l1_uri command with
         | Ok _ ->
             let txn_hash =
               Mina_transaction.Transaction_hash.hash_command
                 (Zkapp_command command)
             in
             [%log info] "Successfully sent zkapp command: %s"
               (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
         | Error (`Failed_request err) ->
             [%log error] "Failed request: %s" err
         | Error (`Graphql_error err) ->
             [%log error] "Graphql request: %s" err ) )

let migrate =
  ( "migrate"
  , Command.async ~summary:"Run migrations on the database"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and postgres_uri =
         flag "--postgres-uri" (required string) ~doc:"string Postgres URI"
       and target_version =
         flag "--target-version" (optional int) ~doc:"int Target version"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let postgres_uri = Uri.of_string postgres_uri in
         let pool =
           Relational_db.Db.create_pool ~postgres_uri ()
           |> Relational_db.caqti_ok_exn ~msg:"Failed to create db pool: %s"
         in
         Relational_db.Db.Migration.run ~logger
           ~target_version:
             (match target_version with None -> `Latest | Some v -> `Version v)
           pool Db.migrations
         >>| Relational_db.caqti_ok_exn ~msg:"Failed to run migrations: %s" ) )

let dump_ledger =
  ( "dump-ledger"
  , Command.basic ~summary:"Dump the ledger"
      (let%map_open.Command target =
         flag "--target" (required string) ~doc:"string Target file json"
       and ledger_dir =
         flag "--ledger-dir" (required string) ~doc:"string Ledger directory"
       in
       fun () ->
         let out = Stdio.Out_channel.create target in
         Stdio.Out_channel.output_string out "[" ;

         let db =
           Ledger.Db.create ~directory_name:ledger_dir
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         in
         Ledger.Db.iteri db ~f:(fun index account ->
             let str =
               Yojson.Safe.to_string
                 ([%to_yojson: int * Account.t] (index, account))
             in
             Stdio.Out_channel.output_string out str ;
             if index < Ledger.Db.num_accounts db - 1 then
               Stdio.Out_channel.output_string out "," ) ;

         Stdio.Out_channel.output_string out "]" ;
         Stdio.Out_channel.close out ) )

let prover_load =
  ( "prover-load"
  , Command.async ~summary:"Dump the ledger"
      (let%map_open.Command mq_host =
         flag "--mq-host" (required string) ~doc:"string Message queue host"
       and count =
         flag "--count"
           (optional_with_default 1 int)
           ~doc:"int Count of transactions"
       in
       fun () ->
         let logger = Logger.create () in
         let%bind client =
           Zeko_prover.Client.create ?db_pool:None ~logger
             ~mq_host:(Host_and_port.of_string mq_host)
         in
         let open Zeko_types in
         let witness : Base_input.serializable =
           let open Test_spec in
           let Test_spec.{ init_ledger; specs } =
             Quickcheck.random_value @@ Test_spec.mk_gen ~num_transactions:5 ()
           in
           let ledger =
             Ledger.with_ledger
               ~depth:Zeko_constants.constraint_constants.ledger_depth
               ~f:(fun ledger ->
                 List.iter (Array.to_list init_ledger)
                   ~f:(fun (keypair, balance) ->
                     let pk =
                       Signature_lib.Public_key.compress keypair.public_key
                     in
                     let account_id = Account_id.create pk Token_id.default in
                     let balance = Unsigned.UInt64.of_int64 balance in
                     let account =
                       Account.create account_id
                         (Currency.Balance.of_uint64 balance)
                     in
                     Ledger.create_new_account_exn ledger account_id account ) ;
                 Ledger.commit ledger ;
                 ledger )
           in
           let imt =
             let db =
               Indexed_merkle_tree.Db.create
                 ~depth:Zeko_constants.constraint_constants.ledger_depth ()
             in
             let tids =
               List.map (Array.to_list init_ledger) ~f:(fun (keypair, _) ->
                   let pk =
                     Signature_lib.Public_key.compress keypair.public_key
                   in
                   let owner = Account_id.create pk Token_id.default in
                   Account_id.derive_token_id ~owner )
             in
             List.iter tids ~f:(fun tid ->
                 let _, _ =
                   Indexed_merkle_tree.Db.get_or_create_entry_exn db tid
                 in
                 () ) ;
             db
           in
           let command =
             command_send ~chain:Mina_signature_kind.Testnet (List.hd_exn specs)
           in
           let source_acc_set =
             Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
           in
           let update_acc_set_witness =
             let fee_payer = Signed_command.fee_payer command in
             let receiver = Signed_command.receiver command in
             let tids =
               List.map [ fee_payer; receiver; fee_payer ] ~f:(fun owner ->
                   Account_id.derive_token_id ~owner )
             in
             List.fold tids ~init:Acc_set_witness.empty ~f:(fun acc tid ->
                 let _, witness =
                   Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
                 in
                 Acc_set_witness.add acc witness )
           in
           { source_ledger = Ledger.merkle_root ledger
           ; source_acc_set
           ; sequencer =
               Even_PC.create_exn
               @@ Public_key.compress
                    (Even_PC.generate_even_signer ()).public_key
           ; transaction = Command command
           ; witness =
               { ledger_path_handler =
                   (let aids = Signed_command.accounts_referenced command in
                    let sparse_ledger =
                      Sparse_ledger.of_ledger_subset_exn ledger aids
                    in
                    sparse_ledger )
               ; update_acc_set_witness
               }
           }
         in
         let witnesses = List.init count ~f:(fun _ -> witness) in
         let%map () =
           Deferred.List.iteri ~how:`Sequential witnesses ~f:(fun i witness ->
               printf "Proving %d/%d\n" i count ;
               let%map _result =
                 Zeko_prover.Client.transaction_snark client
                   (Signed_command witness)
               in
               () )
         in
         print_endline "Done ✅" ) )

let () =
  Command.group ~summary:"Sequencer CLI"
    [ generate_even_key
    ; generate_circuits_config
    ; update_outer_verification_keys
    ; update_inner_verification_keys
    ; update_da_key
    ; update_permissions
    ; set_pause
    ; migrate
    ; dump_ledger
    ; prover_load
    ]
  |> Command_unix.run
