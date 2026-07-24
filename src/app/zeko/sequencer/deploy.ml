open Core
open Mina_base
open Async
open Mina_ledger
open Signature_lib
open Zeko_types
module L = Ledger
module Field = Snark_params.Tick.Field

let constraint_constants = Zeko_constants.constraint_constants

let print_endline = Core.print_endline

let write_deploy_command ~output command =
  Zkapp_command.read_all_proofs_from_disk command
  |> Zkapp_command.Stable.Latest.to_yojson |> Yojson.Safe.to_file output

let read_deploy_command ~input =
  let command =
    match
      Yojson.Safe.from_file input |> Zkapp_command.Stable.Latest.of_yojson
    with
    | Ok command ->
        command
    | Error err ->
        failwithf "Failed to parse deploy command from %s: %s" input err ()
  in
  Zkapp_command.write_all_proofs_to_disk
    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
    ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
    command

let generate ~l1_uri ~sender_pk ~ledger_input ~faucet_aid ~pause_key
    ~sequencer_key ~da_keys ~da_quorum ~account_creation_fee ~prefund_account
    ~da_nodes () =
  let logger = Logger.create () in

  let outer_pk = Zeko_circuits_config.t.zeko_l1 in
  let holder_pk = List.hd_exn Zeko_circuits_config.t.holder_accounts_l1 in
  let token_holder_pk = Zeko_circuits_config.t.helper_token_owner_l1 in

  printf "outer public key: %s\n%!"
    Public_key.(Compressed.to_base58_check outer_pk) ;
  printf "holder public key: %s\n%!"
    Public_key.(Compressed.to_base58_check holder_pk) ;
  printf "token holder public key: %s\n%!"
    Public_key.(Compressed.to_base58_check token_holder_pk) ;

  Thread_safe.block_on_async_exn (fun () ->
      let%bind nonce =
        Gql_client.infer_nonce ~logger l1_uri sender_pk >>| Or_error.ok_exn
      in
      let%bind ( `Inner inner_account
                , `Holder holder_account
                , `Ethereum_asset_registry registry_account ) =
        Sequencer_lib.Deploy.Z.Inner.initial_accounts ()
      in
      let old_ledger_witness, new_ledger, imt_hash, imt =
        let ledger =
          L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
        in
        match ledger_input with
        | None ->
            let sequencer_account =
              { Account.empty with public_key = Even_PC.to_pc sequencer_key }
            in
            let fee_recipient_account =
              { Account.empty with
                public_key = Zeko_circuits_config.Inputs.bridge_fee_recipient_l2
              }
            in
            let genesis_accounts =
              [ inner_account; holder_account ]
              @ Option.to_list registry_account
              @ [ sequencer_account; fee_recipient_account ]
              @ ( match prefund_account with
                | Some (signer, amount) ->
                    [ { Account.empty with
                        public_key = signer
                      ; balance = amount
                      }
                    ]
                | None ->
                    [] )
              @
              match faucet_aid with
              | Some faucet_aid ->
                  [ { Account.empty with
                      public_key = Account_id.public_key faucet_aid
                    ; token_id = Account_id.token_id faucet_aid
                    ; balance = Currency.Balance.max_int
                    }
                  ]
              | None ->
                  []
            in
            List.iter genesis_accounts ~f:(fun acc ->
                L.create_new_account_exn ledger
                  (Account_id.create acc.public_key acc.token_id)
                  acc ) ;

            let tids =
              List.map genesis_accounts ~f:(fun acc ->
                  Account_id.derive_token_id ~owner:(Account.identifier acc) )
            in
            printf "Creating imt\n%!" ;
            let imt, _witnesses =
              Indexed_merkle_tree.Db.create_of_entries_exn
                ~depth:constraint_constants.ledger_depth tids
            in
            let imt_hash =
              Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
            in
            (None, ledger, imt_hash, imt)
        | Some ledger_input_json ->
            print_endline "(* Load ledger from json file *)" ;
            Yojson.Safe.from_file ledger_input_json
            |> Yojson.Safe.Util.to_list
            |> List.map ~f:[%of_yojson: int * Account.t]
            |> List.map ~f:(function Ok x -> x | Error e -> failwith e)
            |> List.iter ~f:(fun (index, account) ->
                   L.set_at_index_exn ledger index account ) ;

            let old_ledger_hash = L.merkle_root ledger in
            printf "Old ledger hash: %s\n%!"
              (Ledger_hash.to_decimal_string old_ledger_hash) ;

            let old_ledger_openings =
              Sparse_ledger.of_ledger_subset_exn ledger
                ( Zeko_constants.inner_account_id
                :: (match faucet_aid with None -> [] | Some aid -> [ aid ]) )
            in

            print_endline "(* Overwrite initial accounts *)" ;
            List.iteri [ inner_account; holder_account ] ~f:(fun i acc ->
                L.set_at_index_exn ledger i acc ) ;

            let registry_account_diff =
              Option.bind registry_account ~f:(fun registry_account ->
                  let account_id = Account.identifier registry_account in
                  match L.location_of_account ledger account_id with
                  | Some _ ->
                      None
                  | None ->
                      L.create_new_account_exn ledger account_id
                        registry_account ;
                      Some
                        ( L.index_of_account_exn ledger account_id
                        , registry_account ) )
            in
            let accounts_diff =
              (0, inner_account) :: (1, holder_account)
              ::
              ( Option.to_list registry_account_diff
              @
              match faucet_aid with
              | None ->
                  []
              | Some faucet_aid ->
                  let account =
                    Account.create faucet_aid Currency.Balance.max_int
                  in
                  let location = L.location_of_account ledger faucet_aid in
                  let () =
                    match location with
                    | None ->
                        L.create_new_account_exn ledger faucet_aid account
                    | Some location ->
                        L.set ledger location account
                  in
                  let index = L.index_of_account_exn ledger faucet_aid in
                  [ (index, account) ] )
            in

            printf "New ledger hash: %s\n%!"
              (Ledger_hash.to_decimal_string @@ L.merkle_root ledger) ;

            print_endline "(* Construct IMT *)" ;
            printf "Creating imt\n%!" ;
            let tids =
              L.to_list_sequential ledger
              |> List.map ~f:Account.identifier
              |> List.map ~f:(fun aid -> Account_id.derive_token_id ~owner:aid)
            in
            let imt, _witnesses =
              Indexed_merkle_tree.Db.create_of_entries_exn
                ~depth:constraint_constants.ledger_depth tids
            in
            let imt_hash =
              let imt_hash = Indexed_merkle_tree.Db.merkle_root imt in
              printf "IMT hash: %s\n%!" (Ledger_hash.to_decimal_string imt_hash) ;
              Account_set.of_fields [| imt_hash |]
            in
            ( Some (old_ledger_hash, old_ledger_openings, accounts_diff)
            , ledger
            , imt_hash
            , imt )
      in
      let%bind command =
        Sequencer_lib.Deploy.deploy_command_exn
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~signer_pk:sender_pk ~outer_pk ~holder_pk ~token_holder_pk
          ~fee:(Currency.Fee.of_mina_int_exn 1)
          ~nonce ~account_creation_fee ~initial_ledger:new_ledger
          ~account_set_hash:imt_hash ~pause_key ~sequencer:sequencer_key
          ~da_key:
            (Multisig.commit
               { public_keys =
                   List.sort da_keys ~compare:Public_key.Compressed.compare
               ; quorum = Field.of_int da_quorum
               } )
          ~prefund_amount:
            ( Option.map prefund_account ~f:snd
            |> Option.value ~default:Currency.Balance.zero
            |> Currency.Balance.to_amount )
          ()
      in

      let da_config = Da_layer.Client.Config.{ nodes = da_nodes } in

      (* If the old ledger exists, we need to just post the diff with updated inner account *)
      let old_ledger_hash = Option.map old_ledger_witness ~f:fst3 in
      let%bind old_ledger_exists =
        match old_ledger_witness with
        | Some (ledger_hash, _, _) ->
            Da_layer.Client.get_diff ~logger ~config:da_config ~ledger_hash
            >>| Result.is_ok
        | None ->
            return false
      in
      let old_and_new_ledger_same =
        (let%map.Option old_ledger_hash = old_ledger_hash in
         Ledger_hash.equal old_ledger_hash (L.merkle_root new_ledger) )
        |> Option.value ~default:false
      in

      print_endline "(* Post genesis batch *)" ;
      let%bind () =
        if List.length da_nodes = 0 then return ()
        else if old_ledger_exists && old_and_new_ledger_same then return ()
        else if old_ledger_exists then
          let () =
            print_endline "(* Post only diff with updated inner account *)"
          in
          let old_ledger_openings, changed_accounts =
            match old_ledger_witness with
            | Some (_, old_ledger_openings, diff) ->
                (old_ledger_openings, diff)
            | None ->
                failwith "Unreachable"
          in
          let diff =
            Da_layer.Diff.create_pending
              ~source_ledger_hash:
                (Sparse_ledger.merkle_root old_ledger_openings)
              ~changed_accounts ~actions:(`Actions [])
          in
          let new_accounts_keys =
            List.filter changed_accounts ~f:(fun (index, _) ->
                Account.equal
                  (Sparse_ledger.get_exn old_ledger_openings index)
                  Account.empty )
            |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
            |> List.map ~f:(fun (_, account) ->
                   Account_id.derive_token_id
                     ~owner:(Account.identifier account) )
          in
          Da_layer.Client.distribute_diff ~logger ~config:da_config
            ~ledger_openings:old_ledger_openings
            ~acc_set_openings:
              (Indexed_merkle_tree.Sparse.of_db_subset ~logger ~db:imt
                 ~keys:new_accounts_keys )
            ~diff
        else
          let () =
            print_endline
              "(* Post the whole genesis diff with all the accounts *)"
          in
          Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
            ~ledger:new_ledger ~get_actions_for_aid:(fun _aid -> [])
      in
      return command )

let deploy ~l1_uri ~sk ~command () =
  let logger = Logger.create () in
  Thread_safe.block_on_async_exn (fun () ->
      let sender_keypair =
        Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
      in
      let deploy_config =
        Option.value_exn ~message:"ZEKO_DEPLOY_CONFIG is not set"
          Zeko_circuits_config.deploy_config
      in
      let outer_kp = Keypair.of_private_key_exn deploy_config.zeko_l1 in
      let holder_kp =
        Keypair.of_private_key_exn
        @@ List.hd_exn deploy_config.holder_accounts_l1
      in
      let token_holder_kp =
        Keypair.of_private_key_exn deploy_config.helper_token_owner_l1
      in
      let command =
        Utils.sign_zkapp_command
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 command
          [ outer_kp; holder_kp; token_holder_kp; sender_keypair ]
      in

      print_endline "(* Deploy contract *)" ;
      match%map
        Gql_client.send_zkapp l1_uri
          (Zkapp_command.read_all_proofs_from_disk command)
      with
      | Ok _ ->
          let txn_hash =
            Mina_transaction.Transaction_hash.hash_command
              (Zkapp_command (Zkapp_command.read_all_proofs_from_disk command))
          in
          [%log info] "Successfully sent zkapp command: %s"
            (Mina_transaction.Transaction_hash.to_base58_check txn_hash)
      | Error (`Failed_request err) ->
          [%log error] "Failed request: %s" err
      | Error (`Graphql_error err) ->
          [%log error] "Graphql error: %s" err )

let deploy_all =
  ( "deploy-all"
  , Command.basic ~summary:"Deploy zeko zkapp"
      (let%map_open.Command l1_uri =
         flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and ledger_input =
         flag "--ledger-input" (optional string)
           ~doc:"string Path to the json dump of the ledger"
       and faucet_account =
         flag "--faucet-account" (optional string)
           ~doc:"string Faucet public key"
       and da_nodes =
         flag "--da-node" (listed string)
           ~doc:"string Address of the DA node, can be supplied multiple times"
       and pause_key =
         flag "--pause-key" (required string) ~doc:"string Pause key"
       and sequencer_key =
         flag "--sequencer-key" (required string) ~doc:"string Sequencer key"
       and da_keys =
         flag "--da-keys" (required string)
           ~doc:"string List of DA keys, separated by commas"
       and da_quorum =
         flag "--da-quorum" (required int)
           ~doc:"int Quorum for the DA signature count"
       and account_creation_fee =
         flag "--account-creation-fee" (required string)
           ~doc:"float Account creation fee in mina"
       and prefund_account =
         flag "--prefund-account" (optional string)
           ~doc:"Prefund the signer account with the given amount of mina"
       and prefund_amount =
         flag "--prefund-amount" (optional string)
           ~doc:"float Amount of mina to prefund the signer account with"
       in
       let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
       let sender_pk =
         Public_key.compress
           (Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk)
             .public_key
       in
       let da_nodes =
         List.mapi da_nodes ~f:(fun i uri ->
             Cli_lib.Flag.Types.
               { value = Host_and_port.of_string uri
               ; name = sprintf "da-node-%d" i
               } )
       in
       let faucet_aid =
         Option.map faucet_account ~f:(fun pk ->
             Public_key.Compressed.of_base58_check_exn pk
             |> Public_key.decompress_exn |> Account_id.of_public_key )
       in
       let string_to_even_pc x =
         Public_key.Compressed.of_base58_check_exn x
         |> Zeko_types.Even_PC.create |> Or_error.ok
       in
       let da_keys =
         String.split ~on:',' da_keys
         |> List.map ~f:Public_key.Compressed.of_base58_check_exn
       in
       let pause_key =
         string_to_even_pc pause_key
         |> Option.value_exn ~message:"Pause key odd"
       in
       let sequencer_key =
         string_to_even_pc sequencer_key
         |> Option.value_exn ~message:"Sequencer key odd"
       in
       let account_creation_fee =
         Currency.Fee.of_mina_string_exn account_creation_fee
       in
       let prefund_account =
         match (prefund_account, prefund_amount) with
         | Some signer, Some amount ->
             Some
               ( Public_key.of_base58_check_decompress_exn signer
               , Currency.Balance.of_mina_string_exn amount )
         | None, None ->
             None
         | _ ->
             failwith
               "Either both or neither of prefund-signer and prefund-amount \
                must be provided"
       in
       let l1_uri = Uri.of_string l1_uri in
       let command =
         generate ~l1_uri ~ledger_input ~faucet_aid ~da_nodes ~pause_key
           ~sequencer_key ~da_keys ~da_quorum ~account_creation_fee
           ~prefund_account ~sender_pk ()
       in
       deploy ~l1_uri ~sk ~command ) )

let generate_deploy_command =
  ( "generate-deploy-command"
  , Command.basic ~summary:"Generate zeko deploy zkapp command"
      (let%map_open.Command l1_uri =
         flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and sender_pk =
         flag "--sender-pk" (required string) ~doc:"string Fee payer public key"
       and ledger_input =
         flag "--ledger-input" (optional string)
           ~doc:"string Path to the json dump of the ledger"
       and faucet_account =
         flag "--faucet-account" (optional string)
           ~doc:"string Faucet public key"
       and da_nodes =
         flag "--da-node" (listed string)
           ~doc:"string Address of the DA node, can be supplied multiple times"
       and pause_key =
         flag "--pause-key" (required string) ~doc:"string Pause key"
       and sequencer_key =
         flag "--sequencer-key" (required string) ~doc:"string Sequencer key"
       and da_keys =
         flag "--da-keys" (required string)
           ~doc:"string List of DA keys, separated by commas"
       and da_quorum =
         flag "--da-quorum" (required int)
           ~doc:"int Quorum for the DA signature count"
       and account_creation_fee =
         flag "--account-creation-fee" (required string)
           ~doc:"float Account creation fee in mina"
       and prefund_account =
         flag "--prefund-account" (optional string)
           ~doc:"Prefund the signer account with the given amount of mina"
       and prefund_amount =
         flag "--prefund-amount" (optional string)
           ~doc:"float Amount of mina to prefund the signer account with"
       and command_output =
         flag "--command-output" (required string)
           ~doc:"string Path to write the generated deploy command JSON"
       in
       let sender_pk = Public_key.Compressed.of_base58_check_exn sender_pk in
       let da_nodes =
         List.mapi da_nodes ~f:(fun i uri ->
             Cli_lib.Flag.Types.
               { value = Host_and_port.of_string uri
               ; name = sprintf "da-node-%d" i
               } )
       in
       let faucet_aid =
         Option.map faucet_account ~f:(fun pk ->
             Public_key.Compressed.of_base58_check_exn pk
             |> Public_key.decompress_exn |> Account_id.of_public_key )
       in
       let string_to_even_pc x =
         Public_key.Compressed.of_base58_check_exn x
         |> Zeko_types.Even_PC.create |> Or_error.ok
       in
       let da_keys =
         String.split ~on:',' da_keys
         |> List.map ~f:Public_key.Compressed.of_base58_check_exn
       in
       let pause_key =
         string_to_even_pc pause_key
         |> Option.value_exn ~message:"Pause key odd"
       in
       let sequencer_key =
         string_to_even_pc sequencer_key
         |> Option.value_exn ~message:"Sequencer key odd"
       in
       let account_creation_fee =
         Currency.Fee.of_mina_string_exn account_creation_fee
       in
       let prefund_account =
         match (prefund_account, prefund_amount) with
         | Some signer, Some amount ->
             Some
               ( Public_key.of_base58_check_decompress_exn signer
               , Currency.Balance.of_mina_string_exn amount )
         | None, None ->
             None
         | _ ->
             failwith
               "Either both or neither of prefund-signer and prefund-amount \
                must be provided"
       in
       let l1_uri = Uri.of_string l1_uri in
       let command =
         generate ~l1_uri ~ledger_input ~faucet_aid ~da_nodes ~pause_key
           ~sequencer_key ~da_keys ~da_quorum ~account_creation_fee
           ~prefund_account ~sender_pk ()
       in
       fun () -> write_deploy_command ~output:command_output command ) )

let deploy_command =
  ( "deploy-command"
  , Command.basic ~summary:"Sign and deploy a generated zeko deploy command"
      (let%map_open.Command l1_uri =
         flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and command_input =
         flag "--command-input" (required string)
           ~doc:"string Path to the generated deploy command JSON"
       in
       let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
       let l1_uri = Uri.of_string l1_uri in
       let command = read_deploy_command ~input:command_input in
       deploy ~l1_uri ~sk ~command ) )

let deploy_token_owner =
  ( "deploy-token-owner"
  , Command.basic ~summary:"Deploy zeko token owner"
      (let%map_open.Command l1_uri =
         flag "--l1-uri" (required string) ~doc:"string L1 URI"
       and account_creation_fee =
         flag "--account-creation-fee" (required string)
           ~doc:"float Account creation fee in mina"
       in
       fun () ->
         let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let account_creation_fee =
           Currency.Fee.of_mina_string_exn account_creation_fee
         in
         let l1_uri = Uri.of_string l1_uri in

         let sender_keypair =
           Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
         in
         let deploy_config =
           Option.value_exn ~message:"ZEKO_DEPLOY_CONFIG is not set"
             Zeko_circuits_config.deploy_config
         in
         let token_owner_kp =
           Keypair.of_private_key_exn deploy_config.helper_token_owner_l1
         in
         let logger = Logger.create () in
         Thread_safe.block_on_async_exn (fun () ->
             let%bind nonce =
               Gql_client.infer_nonce ~logger l1_uri
                 (Public_key.compress sender_keypair.public_key)
               >>| Or_error.ok_exn
             in
             let%bind command =
               Sequencer_lib.Deploy.deploy_token_owner_exn
                 ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                 ~signer:sender_keypair ~token_owner_kp
                 ~fee:(Currency.Fee.of_mina_int_exn 1)
                 ~nonce ~account_creation_fee ()
             in
             match%bind
               Gql_client.send_zkapp l1_uri
                 (Zkapp_command.read_all_proofs_from_disk command)
             with
             | Ok _ ->
                 Deferred.unit
             | Error (`Failed_request err) ->
                 eprintf "Failed request: %s\n%!" err ;
                 Deferred.unit
             | Error (`Graphql_error err) ->
                 eprintf "Graphql error: %s\n%!" err ;
                 Deferred.unit ) ) )

let () =
  Command.group ~summary:"Sequencer CLI"
    [ deploy_all; generate_deploy_command; deploy_command; deploy_token_owner ]
  |> Command_unix.run
