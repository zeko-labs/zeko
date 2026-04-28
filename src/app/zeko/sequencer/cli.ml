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

let default_admin_permissions : Permissions.t =
  { edit_state = Either
  ; send = Proof
  ; receive = None
  ; set_delegate = Proof
  ; set_permissions = Proof
  ; set_verification_key = (Proof, Mina_numbers.Txn_version.current)
  ; set_zkapp_uri = Proof
  ; edit_action_state = Proof
  ; set_token_symbol = Proof
  ; increment_nonce = Proof
  ; set_voting_for = Proof
  ; set_timing = Proof
  ; access = None
  }

let load_json_file path f =
  Yojson.Safe.from_file path |> f |> Result.ok_or_failwith

let write_json ?output json =
  match output with
  | None ->
      Core.printf "%s\n%!" (Yojson.Safe.pretty_to_string json)
  | Some output ->
      Yojson.Safe.to_file output json

let write_signed_multisig_updates ?output updates =
  write_json ?output
    (`List (List.map updates ~f:Deploy.Signed_multisig_update.to_yojson))

let send_direct ~logger ~l1_uri ~(signers : Keypair.t list)
    ~(fee_signer : Keypair.t) ~bodies =
  let signature_kind = Zeko_circuits_config.t.chain_l1 in
  let%bind nonce =
    Gql_client.infer_nonce ~logger l1_uri
      (Public_key.compress fee_signer.public_key)
    >>| Or_error.ok_exn
  in
  let account_updates =
    List.map bodies ~f:(fun body ->
        let body =
          { body with
            Account_update.Body.authorization_kind =
              Account_update.Authorization_kind.Signature
          }
        in
        Account_update.with_aux ~body
          ~authorization:(Control.Poly.Signature Signature.dummy) )
    |> Zkapp_command.Call_forest.of_account_updates
         ~account_update_depth:(fun _ -> 0)
    |> Utils.rehash_forest ~signature_kind
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress fee_signer.public_key
            ; fee = Currency.Fee.of_mina_string_exn "0.1"
            ; valid_until = None
            ; nonce
        ; authorization = Signature.dummy
    }
  in
  let command =
    Utils.sign_zkapp_command ~signature_kind command ( fee_signer :: signers)
    |> Zkapp_command.read_all_proofs_from_disk
  in
  match%map Gql_client.send_zkapp l1_uri command with
  | Ok _ ->
      let txn_hash =
        Mina_transaction.Transaction_hash.hash_command (Zkapp_command command)
      in
      [%log info] "Successfully sent zkapp command: %s"
        (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
  | Error (`Failed_request err) ->
      [%log error] "Failed request: %s" err
  | Error (`Graphql_error err) ->
      [%log error] "Graphql request: %s" err

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

let construct_multisig_key =
  ( "construct-multisig-key"
  , Command.basic ~summary:"Construct a multisig key from a list of public keys"
      (let%map_open.Command public_keys =
         flag "--public-key" (listed string) ~doc:"string Public key"
       and quorum = flag "--quorum" (required int) ~doc:"int Quorum" in
       fun () ->
         let public_keys =
           List.map public_keys ~f:Public_key.Compressed.of_base58_check_exn
         in
         let multisig_key =
           Zeko_circuits.Multisig.commit
             { public_keys; quorum = Field.of_int quorum }
         in
         Core.printf "Multisig key: %s\n" (Field.to_string multisig_key) ) )

let generate_circuits_config =
  ( "generate-circuits-config"
  , Command.basic ~summary:"Generate the circuits config and deploy config"
      (let%map_open.Command circuits_config_output =
         flag "--circuits-config-output" (optional string)
           ~doc:"string Circuits config output"
       and deploy_config_output =
         flag "--deploy-config-output" (optional string)
           ~doc:"string Deploy config output"
       in
       fun () ->
         let generate_keypair () =
           let kp = Keypair.create () in
           (Public_key.compress kp.public_key, kp.private_key)
         in
         let holder_accounts_l1 = [ generate_keypair () ] in
         let helper_token_owner_l1 = generate_keypair () in
         let zeko_l1 = generate_keypair () in
         let emergency_da = generate_keypair () in
         let bridge_fee_recipient_l1 = generate_keypair () in
         let bridge_fee_recipient_l2 = generate_keypair () in
         let t : Zeko_circuits_config.t =
           { chain_l1 = Testnet
           ; chain_l2 = Testnet
           ; max_valid_while_size = Zeko_circuits.Zeko_util.Slot.max_value
           ; multisig_key =
               { public_keys = List.map holder_accounts_l1 ~f:fst
               ; quorum = Field.of_int 1
               }
           ; holder_accounts_l1 = List.map holder_accounts_l1 ~f:fst
           ; helper_token_owner_l1 = fst helper_token_owner_l1
           ; zeko_l1 = fst zeko_l1
           ; emergency_da_public_key = fst emergency_da
           ; withdrawal_delay = Mina_numbers.Global_slot_span.of_int 5
           ; bridge_fee_recipient_l1 = fst bridge_fee_recipient_l1
           ; bridge_fee_recipient_l2 = fst bridge_fee_recipient_l2
           }
         in
         let deploy_config : Zeko_circuits_config.Deploy.t =
           { holder_accounts_l1 = List.map holder_accounts_l1 ~f:snd
           ; helper_token_owner_l1 = snd helper_token_owner_l1
           ; zeko_l1 = snd zeko_l1
           ; emergency_da = snd emergency_da
           ; bridge_fee_recipient_l1 = snd bridge_fee_recipient_l1
           ; bridge_fee_recipient_l2 = snd bridge_fee_recipient_l2
           }
         in
         let circuits_config_json = Zeko_circuits_config.to_yojson t in
         let deploy_config_json =
           Zeko_circuits_config.Deploy.to_yojson deploy_config
         in
         let () =
           match circuits_config_output with
           | None ->
               Core.printf "circuits config: %s\n%!"
                 (Yojson.Safe.pretty_to_string circuits_config_json)
           | Some output_file ->
               Yojson.Safe.to_file output_file circuits_config_json
         in
         match deploy_config_output with
         | None ->
             Core.printf "deploy config: %s\n%!"
               (Yojson.Safe.pretty_to_string deploy_config_json)
         | Some output_file ->
             Yojson.Safe.to_file output_file deploy_config_json ) )

let update_outer_verification_keys =
  ( "update-outer-verification-keys"
  , Command.async
      ~summary:
        "Build and sign multisig updates for the outer zkApp verification keys"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and output =
         flag "--output" (optional string)
           ~doc:"string Output signed update JSON file"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       and direct =
         flag "--direct" no_arg
           ~doc:
             "bool Build and send the zkapp command directly instead of \
              signing for multisig"
       in
       fun () ->
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
           Gql_client.fetch_vk ~logger l1_uri
             ( Account_id.of_public_key
             @@ Public_key.decompress_exn Zeko_circuits_config.t.zeko_l1 )
           >>| Or_error.ok_exn >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         and fetched_bridge_holder_vk =
           Gql_client.fetch_vk ~logger l1_uri
             ( Account_id.of_public_key @@ Public_key.decompress_exn
             @@ List.hd_exn Zeko_circuits_config.t.holder_accounts_l1 )
           >>| Or_error.ok_exn >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         and fetched_helper_token_owner_vk =
           Gql_client.fetch_vk ~logger l1_uri
             ( Account_id.of_public_key
             @@ Public_key.decompress_exn
                  Zeko_circuits_config.t.helper_token_owner_l1 )
           >>| Or_error.ok_exn >>| Compile_simple.Verification_key.of_pickles
           >>| Compile_simple.Verification_key.hash
         in
         let%bind outer_vk =
           Lazy.force Outer_rules_inst.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         and bridge_holder_enabled_vk =
           Lazy.force Bridge_inst_mina.System_L1_enabled.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         and helper_token_owner_vk =
           Lazy.force Bridge_inst_mina.System_L1_token_owner.tag
           |> Compile_simple.Verification_key.of_tag |> Promise.to_deferred
         in
         let outer =
           (outer_vk, fetched_outer_vk, Zeko_circuits_config.t.zeko_l1)
         in
         let bridge_holders =
           List.map Zeko_circuits_config.t.holder_accounts_l1 ~f:(fun pk ->
               (bridge_holder_enabled_vk, fetched_bridge_holder_vk, pk) )
         in
         let helper_token_owner =
           ( helper_token_owner_vk
           , fetched_helper_token_owner_vk
           , Zeko_circuits_config.t.helper_token_owner_l1 )
         in
         pp "Core rollup" (take2 outer) ;
         pp "Bridge holder" (take2 @@ List.hd_exn bridge_holders) ;
         pp "Helper token owner" (take2 helper_token_owner) ;

         if only_check then return ()
         else
           let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
           let signer =
             Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
           in
           let to_update =
             List.filter_map
               ([ outer; helper_token_owner ] @ bridge_holders)
               ~f:(fun (new_vk, old_vk, kp) ->
                 if
                   Field.equal
                     (Compile_simple.Verification_key.hash new_vk)
                     old_vk
                 then None
                 else Some (new_vk, kp) )
           in
           if direct then
             let%bind bodies =
               Deferred.List.map to_update ~how:`Sequential
                 ~f:(fun (new_vk, pk) ->
                   let kind =
                     if
                       Public_key.Compressed.equal pk
                         Zeko_circuits_config.t.zeko_l1
                     then Deploy.Multisig_update_kind.Outer
                     else if
                       Public_key.Compressed.equal pk
                         Zeko_circuits_config.t.helper_token_owner_l1
                     then Deploy.Multisig_update_kind.Bridge_token_owner_l1
                     else Deploy.Multisig_update_kind.Bridge_holder_l1_enabled
                   in
                   Deploy.build_verification_key_multisig_update_body ~kind
                     ~public_key:pk ~verification_key:new_vk )
             in
             let deploy_config =
               Option.value_exn Zeko_circuits_config.deploy_config
                 ~message:"Deploy config not found"
             in
             send_direct ~logger ~l1_uri
               ~signers:
                 ( [ signer
                   ; Keypair.of_private_key_exn
                       deploy_config.helper_token_owner_l1
                   ; Keypair.of_private_key_exn deploy_config.zeko_l1
                   ; Keypair.of_private_key_exn deploy_config.emergency_da
                   ]
                 @ List.map deploy_config.holder_accounts_l1
                     ~f:Keypair.of_private_key_exn )
               ~fee_signer:signer ~bodies
           else
             let%map signed_updates =
               Deferred.List.map to_update ~how:`Sequential
                 ~f:(fun (new_vk, pk) ->
                   let kind =
                     if
                       Public_key.Compressed.equal pk
                         Zeko_circuits_config.t.zeko_l1
                     then Deploy.Multisig_update_kind.Outer
                     else if
                       Public_key.Compressed.equal pk
                         Zeko_circuits_config.t.helper_token_owner_l1
                     then Deploy.Multisig_update_kind.Bridge_token_owner_l1
                     else Deploy.Multisig_update_kind.Bridge_holder_l1_enabled
                   in
                   let%map body =
                     Deploy.build_verification_key_multisig_update_body ~kind
                       ~public_key:pk ~verification_key:new_vk
                   in
                   Deploy.sign_multisig_update ~signer ~kind ~body )
             in
             write_signed_multisig_updates ?output signed_updates ) )

let update_inner_verification_keys =
  ( "update-inner-verification-keys"
  , Command.async
      ~summary:
        "Build and sign the outer multisig update needed for inner vk changes"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and da_node = flag "--da-node" (required string) ~doc:"string DA node"
       and output =
         flag "--output" (optional string)
           ~doc:"string Output signed update JSON file"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       and db_path =
         flag "--db-path" (optional string)
           ~doc:"string Path to the ledger file"
       and direct =
         flag "--direct" no_arg
           ~doc:
             "bool Build and send the zkapp command directly instead of \
              signing for multisig"
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
           Gql_client.infer_state ~logger l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Or_error.ok_exn
           >>| Utils.value_of_zkapp_state
                 Zeko_circuits.Rollup_state.Outer_state.typ
           >>| fun { ledger_hash; _ } -> ledger_hash
         in
         let da_config = Da_layer.Client.Config.of_string_list [ da_node ] in
         let%bind ledger, imt =
           match db_path with
           | Some db_path ->
               let ledger =
                 Ledger.Db.create ~directory_name:(db_path ^ "/ledger")
                   ~depth:Zeko_constants.constraint_constants.ledger_depth ()
                 |> Ledger.of_database
               in
               let imt =
                 Indexed_merkle_tree.Db.create
                   ~directory_name:(db_path ^ "/imt")
                   ~depth:Zeko_constants.constraint_constants.ledger_depth ()
               in
               return (ledger, imt)
           | None ->
               (* Sync ledger *)
               let ledger =
                 Ledger.create_ephemeral
                   ~depth:Zeko_constants.constraint_constants.ledger_depth ()
               in
               let imt =
                 Indexed_merkle_tree.Db.create
                   ~depth:Zeko_constants.constraint_constants.ledger_depth ()
               in
               let%map () =
                 Da_layer.Client.map_diffs ~logger ~config:da_config
                   ~depth:Zeko_constants.constraint_constants.ledger_depth
                   ~source_ledger_hash:`Genesis
                   ~target_ledger_hash:commited_ledger_hash ()
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
                       |> List.sort ~compare:(fun (a, _) (b, _) ->
                              Int.compare a b )
                     in
                     List.iter changed_accounts ~f:(fun (index, account) ->
                         Ledger.set_at_index_exn ledger index account ) ;
                     (* Add to Indexed Merkle Tree *)
                     List.iter changed_accounts ~f:(fun (_, account) ->
                         let aid = Account.identifier account in
                         let _w =
                           Indexed_merkle_tree.Db.get_or_create_entry_exn imt
                             (Account_id.derive_token_id ~owner:aid)
                         in
                         () ) ;
                     return () )
                 >>| Or_error.ok_exn >>| ignore
               in
               (ledger, imt)
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
         let deploy_config =
           Option.value_exn Zeko_circuits_config.deploy_config
         in
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
           let new_accounts_keys =
             List.filter (Da_layer.Diff.changed_accounts diff)
               ~f:(fun (index, _) ->
                 Account.equal
                   (Sparse_ledger.get_exn ledger_openings index)
                   Account.empty )
             |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
             |> List.map ~f:(fun (_, account) ->
                    Account_id.derive_token_id
                      ~owner:(Account.identifier account) )
           in
           let%bind () =
             Da_layer.Client.distribute_diff ~logger ~config:da_config
               ~ledger_openings
               ~acc_set_openings:
                 (Indexed_merkle_tree.Sparse.of_db_subset ~logger ~db:imt
                    ~keys:new_accounts_keys )
               ~diff
           in
           let%bind body =
             let open Zeko_circuits.Rollup_state in
             Deploy.build_outer_state_multisig_update_body
               ~precondition:
                 Outer_state.
                   { pause_key = None
                   ; status_flags = None
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
                   ; status_flags = None
                   ; ledger_hash =
                       Some (Ledger_hash.var_of_t target_ledger_hash)
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = None
                   ; acc_set = None
                   }
           in
           if direct then
             let deploy_config =
               Option.value_exn Zeko_circuits_config.deploy_config
                 ~message:"Deploy config not found"
             in
             send_direct ~logger ~l1_uri
               ~signers:
                 ( [ sender
                   ; Keypair.of_private_key_exn
                       deploy_config.helper_token_owner_l1
                   ; Keypair.of_private_key_exn deploy_config.zeko_l1
                   ; Keypair.of_private_key_exn deploy_config.emergency_da
                   ]
                 @ List.map deploy_config.holder_accounts_l1
                     ~f:Keypair.of_private_key_exn )
               ~fee_signer:sender ~bodies:[ body ]
           else
             let signed_update =
               Deploy.sign_multisig_update ~signer:sender
                 ~kind:Deploy.Multisig_update_kind.Outer ~body
             in
             return (write_signed_multisig_updates ?output [ signed_update ]) )
  )

let update_da_key =
  ( "update-da-key"
  , Command.async
      ~summary:"Build and sign the outer multisig update for the DA key"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and da_keys =
         flag "--da-key" (listed string) ~doc:"string list of DA keys"
       and da_quorum = flag "--quorum" (required int) ~doc:"int DA quorum"
       and output =
         flag "--output" (optional string)
           ~doc:"string Output signed update JSON file"
       and only_check =
         flag "--only-check" no_arg
           ~doc:"bool Only check if the verification keys are up to date"
       and direct =
         flag "--direct" no_arg
           ~doc:
             "bool Build and send the zkapp command directly instead of \
              signing for multisig"
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
           Gql_client.infer_state ~logger l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Or_error.ok_exn
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
           let%bind body =
             let open Zeko_circuits.Rollup_state in
             Deploy.build_outer_state_multisig_update_body
               ~precondition:
                 Outer_state.
                   { pause_key = None
                   ; status_flags = None
                   ; ledger_hash = None
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = Some (Field.Var.constant current_da_key)
                   ; acc_set = None
                   }
               ~update:
                 Outer_state.
                   { pause_key = None
                   ; status_flags = None
                   ; ledger_hash = None
                   ; inner_action_state = { state = None; length = None }
                   ; sequencer = None
                   ; da_key = Some (Field.Var.constant new_da_key)
                   ; acc_set = None
                   }
           in
           if direct then
             let deploy_config =
               Option.value_exn Zeko_circuits_config.deploy_config
                 ~message:"Deploy config not found"
             in
             send_direct ~logger ~l1_uri
               ~signers:
                 ( [ sender
                   ; Keypair.of_private_key_exn
                       deploy_config.helper_token_owner_l1
                   ; Keypair.of_private_key_exn deploy_config.zeko_l1
                   ; Keypair.of_private_key_exn deploy_config.emergency_da
                   ]
                 @ List.map deploy_config.holder_accounts_l1
                     ~f:Keypair.of_private_key_exn )
               ~fee_signer:sender ~bodies:[ body ]
           else
             let signed_update =
               Deploy.sign_multisig_update ~signer:sender
                 ~kind:Deploy.Multisig_update_kind.Outer ~body
             in
             return (write_signed_multisig_updates ?output [ signed_update ]) )
  )

let update_permissions =
  ( "update-permissions"
  , Command.async
      ~summary:"Build and sign the outer multisig update for permissions"
      (let%map_open.Command output =
         flag "--output" (optional string)
           ~doc:"string Output signed update JSON file"
       and permissions_file =
         flag "--permissions-file" (optional string)
           ~doc:"string Optional JSON file with Permissions.t"
       and l1_uri =
         flag "--l1-uri" (optional string)
           ~doc:"string L1 URI (required with --direct)"
       and direct =
         flag "--direct" no_arg
           ~doc:
             "bool Build and send the zkapp command directly instead of \
              signing for multisig"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let signer =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let logger = Logger.create () in
         let permissions =
           match permissions_file with
           | None ->
               default_admin_permissions
           | Some path ->
               load_json_file path [%of_yojson: Permissions.t]
         in
         let%bind body =
           Deploy.build_permissions_multisig_update_payload ~permissions
         in
         if direct then
           let l1_uri =
             Uri.of_string
               (Option.value_exn ~message:"--l1-uri is required with --direct"
                  l1_uri )
           in
           let deploy_config =
             Option.value_exn Zeko_circuits_config.deploy_config
               ~message:"Deploy config not found"
           in
           send_direct ~logger ~l1_uri
             ~signers:
               ( [ signer
                 ; Keypair.of_private_key_exn
                     deploy_config.helper_token_owner_l1
                 ; Keypair.of_private_key_exn deploy_config.zeko_l1
                 ; Keypair.of_private_key_exn deploy_config.emergency_da
                 ]
               @ List.map deploy_config.holder_accounts_l1
                   ~f:Keypair.of_private_key_exn )
             ~fee_signer:signer ~bodies:[ body.body ]
         else
           let signed_update =
             Deploy.sign_multisig_update ~signer
               ~kind:Deploy.Multisig_update_kind.Outer ~body:body.body
           in
           return (write_signed_multisig_updates ?output [ signed_update ]) ) )

let multisig_submit =
  ( "multisig-submit"
  , Command.async
      ~summary:
        "Collect signed updates, prove the matching multisig branch, and submit"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and signed_update_files =
         flag "--signed-update-file" (listed string)
           ~doc:"string Signed update JSON file; may be repeated"
       in
       fun () ->
         if List.is_empty signed_update_files then
           failwith "--signed-update-file is required" ;
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let sender =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let l1_uri = Uri.of_string l1_uri in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let signed_updates =
           List.concat_map signed_update_files ~f:(fun path ->
               load_json_file path
                 [%of_yojson: Deploy.Signed_multisig_update.t list] )
         in
         let groups =
           List.fold signed_updates ~init:String.Map.empty
             ~f:(fun acc (signed_update : Deploy.Signed_multisig_update.t) ->
               let key =
                 Yojson.Safe.to_string
                   (`List
                     [ Deploy.Multisig_update_kind.to_yojson signed_update.kind
                     ; `String (Field.to_string signed_update.payload)
                     ] )
               in
               Map.update acc key ~f:(function
                 | None ->
                     [ signed_update ]
                 | Some xs ->
                     signed_update :: xs ) )
         in
         let groups = Map.data groups in
         let%map () =
           Deferred.List.iter groups ~how:`Sequential ~f:(fun group ->
               let first = List.hd_exn group in
               let payload =
                 Deploy.Multisig_update_payload.
                   { body = first.body; payload = first.payload }
               in
               let signatures =
                 List.map group ~f:(fun signed_update ->
                     Deploy.Multisig_update_signature.
                       { public_key = signed_update.signer_public_key
                       ; signature = signed_update.signature
                       ; payload = signed_update.payload
                       } )
               in
               let%bind nonce =
                 Gql_client.infer_nonce ~logger l1_uri
                   (Public_key.compress sender.public_key)
                 >>| Or_error.ok_exn
               in
               let%bind command =
                 Deploy.submit_multisig_update ~kind:first.kind ~signer:sender
                   ~fee:(Currency.Fee.of_mina_string_exn "0.1")
                   ~nonce ~payload ~signatures
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
                   [%log error] "Graphql request: %s" err )
         in
         () ) )

let set_pause =
  ( "set-pause"
  , Command.async ~summary:"Build and sign the outer multisig pause update"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and output =
         flag "--output" (optional string)
           ~doc:"string Output signed update JSON file"
       and value =
         flag "--value" (required bool) ~doc:"bool Value to set the pause to"
       and direct =
         flag "--direct" no_arg
           ~doc:
             "bool Build and send the zkapp command directly instead of \
              signing for multisig"
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
           Gql_client.infer_state ~logger l1_uri
             ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
             ~signer_pk:(Public_key.compress sender.public_key)
           >>| Or_error.ok_exn
           >>| Utils.value_of_zkapp_state
                 Zeko_circuits.Rollup_state.Outer_state.typ
           >>| fun { status_flags; _ } ->
           Zeko_circuits.Rollup_state.Outer_state.Status_flags.paused
             status_flags
         in

         [%log info] "Current paused: %b" current_paused ;

         let%bind body =
           let open Zeko_circuits in
           Deploy.build_outer_state_multisig_update_body
             ~precondition:
               Rollup_state.Outer_state.
                 { pause_key = None
                 ; status_flags = None
                 ; ledger_hash = None
                 ; inner_action_state = { state = None; length = None }
                 ; sequencer = None
                 ; da_key = None
                 ; acc_set = None
                 }
             ~update:
               Rollup_state.Outer_state.
                 { pause_key = None
                 ; status_flags =
                     Some
                       (Field.Var.constant
                          (Rollup_state.Outer_state.Status_flags.to_field
                             (Rollup_state.Outer_state.Status_flags.of_bools
                                ~paused:value ~emergency:false ) ) )
                 ; ledger_hash = None
                 ; inner_action_state = { state = None; length = None }
                 ; sequencer = None
                 ; da_key = None
                 ; acc_set = None
                 }
         in
         if direct then
           let deploy_config =
             Option.value_exn Zeko_circuits_config.deploy_config
               ~message:"Deploy config not found"
           in
           send_direct ~logger ~l1_uri
             ~signers:
               ( [ sender
                 ; Keypair.of_private_key_exn
                     deploy_config.helper_token_owner_l1
                 ; Keypair.of_private_key_exn deploy_config.zeko_l1
                 ; Keypair.of_private_key_exn deploy_config.emergency_da
                 ]
               @ List.map deploy_config.holder_accounts_l1
                   ~f:Keypair.of_private_key_exn )
             ~fee_signer:sender ~bodies:[ body ]
         else
           let signed_update =
             Deploy.sign_multisig_update ~signer:sender
               ~kind:Deploy.Multisig_update_kind.Outer ~body
           in
           return (write_signed_multisig_updates ?output [ signed_update ]) ) )

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

let dump_db =
  ( "dump-db"
  , Command.basic ~summary:"Dump the database"
      (let%map_open.Command target =
         flag "--target" (required string) ~doc:"string Target file json"
       and db_path =
         flag "--db-path" (required string) ~doc:"string Database path"
       in
       fun () ->
         let () =
           (* Dump ledger *)
           let out =
             Stdio.Out_channel.create (Filename.concat target "ledger.json")
           in
           Stdio.Out_channel.output_string out "[" ;

           let db =
             Ledger.Db.create ~directory_name:(db_path ^ "/ledger")
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
           Stdio.Out_channel.close out
         in
         let () =
           (* Dump imt *)
           let out =
             Stdio.Out_channel.create (Filename.concat target "imt.json")
           in
           Stdio.Out_channel.output_string out "[" ;

           let db =
             Indexed_merkle_tree.Db.create ~directory_name:(db_path ^ "/imt")
               ~depth:Zeko_constants.constraint_constants.ledger_depth ()
           in
           Indexed_merkle_tree.Db.iteri db ~f:(fun index entry ->
               let str =
                 Yojson.Safe.to_string
                   ([%to_yojson: int * Indexed_merkle_tree.Entry.t]
                      (index, entry) )
               in
               Stdio.Out_channel.output_string out str ;
               if index < Indexed_merkle_tree.Db.num_entries db - 1 then
                 Stdio.Out_channel.output_string out "," ) ;

           Stdio.Out_channel.output_string out "]" ;
           Stdio.Out_channel.close out
         in
         () ) )

let load_db =
  ( "load-db"
  , Command.basic ~summary:"Load the database"
      (let%map_open.Command dump_path =
         flag "--dump-path" (required string) ~doc:"string Dump path"
       and db_path =
         flag "--db-path" (required string) ~doc:"string Database path"
       in
       fun () ->
         let ledger =
           Ledger.Db.create ~directory_name:(db_path ^ "/ledger")
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
           |> Ledger.of_database
         in
         let imt =
           Indexed_merkle_tree.Db.create ~directory_name:(db_path ^ "/imt")
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         in
         Yojson.Safe.from_file (Filename.concat dump_path "ledger.json")
         |> Yojson.Safe.Util.to_list
         |> List.map ~f:[%of_yojson: int * Account.t]
         |> List.map ~f:(function Ok x -> x | Error e -> failwith e)
         |> List.iter ~f:(fun (index, account) ->
                Core.printf
                  !"Loading account %{sexp:Public_key.Compressed.t}\n%!"
                  account.public_key ;
                Ledger.set_at_index_exn ledger index account ;
                let aid = Account.identifier account in
                let tid = Account_id.derive_token_id ~owner:aid in
                let _witness =
                  Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
                in
                () ) ;
         Ledger.commit ledger ;
         Core.printf "Loaded %d accounts\n%!" (Ledger.num_accounts ledger) ) )

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

let sync_ledger =
  ( "sync-ledger"
  , Command.async ~summary:"Sync the ledger"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and da_node = flag "--da-node" (required string) ~doc:"string DA node"
       and ledger_path =
         flag "--ledger-path" (required string) ~doc:"string Ledger path"
       and target_ledger_hash =
         flag "--target-ledger-hash" (required string)
           ~doc:"string Target ledger hash"
       and output_path =
         flag "--output-path" (required string) ~doc:"string Output path"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;

         let da_config = Da_layer.Client.Config.of_string_list [ da_node ] in

         [%log info] "Creating ledger" ;
         let ledger =
           let ledger =
             Ledger.Db.create ~directory_name:ledger_path
               ~depth:Zeko_constants.constraint_constants.ledger_depth ()
           in
           Ledger.Db.create_checkpoint ledger
             ~directory_name:(output_path ^ "/ledger") ()
         in
         let%bind () =
           Da_layer.Client.iter_diffs ~logger ~config:da_config
             ~depth:Zeko_constants.constraint_constants.ledger_depth
             ~source_ledger_hash:(`Specific (Ledger.Db.merkle_root ledger))
             ~target_ledger_hash:
               (Ledger_hash.of_decimal_string target_ledger_hash)
             ()
             ~f:(fun ~current_chunk ~current_diff:_ ~chunks_length diff ->
               assert (
                 Ledger_hash.equal
                   (Da_layer.Diff.Stable.Latest.source_ledger_hash diff)
                   (Ledger.Db.merkle_root ledger) ) ;
               [%log info]
                 "Applying diff with source ledger hash %s, progress: %.0f%%"
                 (Ledger_hash.to_decimal_string
                    (Da_layer.Diff.Stable.Latest.source_ledger_hash diff) )
                 ( Float.of_int current_chunk /. Float.of_int chunks_length
                 *. 100.0 ) ;

               let mask = Ledger.of_database ledger in
               let changed_accounts =
                 Da_layer.Diff.Stable.Latest.changed_accounts diff
                 |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
               in
               List.iter changed_accounts ~f:(fun (index, account) ->
                   Ledger.set_at_index_exn mask index account ) ;
               Ledger.Mask.Attached.commit mask ;

               let () =
                 match
                   Da_layer.Diff.Stable.Latest.command_with_action_step_flags
                     diff
                 with
                 | Some (Zkapp_command command, _) ->
                     Sequencer.apply_events_and_actions ledger
                       (Archive.create ~kvdb:(Ledger.Db.zeko_kvdb ledger))
                       (Zkapp_command.write_all_proofs_to_disk
                          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                          ~proof_cache_db:
                            (Proof_cache_tag.create_identity_db ())
                          command )
                     |> Or_error.ok_exn
                 | _ ->
                     ( (* No events nor actions to add *) )
               in

               return () )
           >>| Or_error.ok_exn
         in
         [%log info] "Synced ledger" ;

         [%log info] "Creating IMT" ;
         let imt =
           Indexed_merkle_tree.Db.create
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         in
         let l = Ledger.Db.num_accounts ledger in
         let () =
           Ledger.Db.iteri ledger ~f:(fun index account ->
               let progress = Float.of_int index /. Float.of_int l *. 100.0 in
               if index mod 200 = 0 then
                 [%log info] "Progress: %.2f%%\t%d/%d" progress index l ;
               let aid = Account.identifier account in
               let tid = Account_id.derive_token_id ~owner:aid in
               let _witness =
                 Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
               in
               () )
         in
         [%log info] "Created IMT" ;
         return () ) )

let () =
  Command.group ~summary:"Sequencer CLI"
    [ generate_even_key
    ; generate_circuits_config
    ; update_outer_verification_keys
    ; update_inner_verification_keys
    ; update_da_key
    ; update_permissions
    ; multisig_submit
    ; set_pause
    ; migrate
    ; dump_db
    ; load_db
    ; prover_load
    ; construct_multisig_key
    ; sync_ledger
    ]
  |> Command_unix.run
