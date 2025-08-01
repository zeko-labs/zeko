open Core
open Mina_base
open Async
open Mina_ledger
open Signature_lib
open Zeko_types
module L = Ledger

let constraint_constants = Zeko_constants.constraint_constants

let print_endline = Core.print_endline

let run ~l1_uri ~sk ~ledger_input ~faucet_aid ~da_nodes ~pause_key
    ~sequencer_key ~da_key ~account_creation_fee () =
  let logger = Logger.create () in
  let sender_keypair =
    Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
  in
  let deploy_config =
    Option.value_exn ~message:"ZEKO_DEPLOY_CONFIG is not set"
      Zeko_circuits_config.deploy_config
  in
  let outer_kp = Keypair.of_private_key_exn deploy_config.zeko_l1 in
  let holder_kp =
    Keypair.of_private_key_exn @@ List.hd_exn deploy_config.holder_accounts_l1
  in
  let token_holder_kp =
    Keypair.of_private_key_exn deploy_config.helper_token_owner_l1
  in
  printf "outer secret key: %s\n%!"
    (Private_key.to_base58_check outer_kp.private_key) ;
  printf "outer public key: %s\n%!"
    Public_key.(Compressed.to_base58_check @@ compress outer_kp.public_key) ;
  printf "holder secret key: %s\n%!"
    (Private_key.to_base58_check holder_kp.private_key) ;
  printf "holder public key: %s\n%!"
    Public_key.(Compressed.to_base58_check @@ compress holder_kp.public_key) ;
  printf "token holder secret key: %s\n%!"
    (Private_key.to_base58_check token_holder_kp.private_key) ;
  printf "token holder public key: %s\n%!"
    Public_key.(
      Compressed.to_base58_check @@ compress token_holder_kp.public_key) ;

  Thread_safe.block_on_async_exn (fun () ->
      let%bind nonce =
        Sequencer_lib.Gql_client.infer_nonce l1_uri
          (Public_key.compress sender_keypair.public_key)
      in
      let%bind `Inner inner_account, `Holder holder_account =
        Sequencer_lib.Deploy.Z.Inner.initial_accounts ()
      in
      let old_ledger_witness, new_ledger, imt_hash =
        let ledger =
          L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
        in
        match ledger_input with
        | None ->
            let sequencer_account =
              { Account.empty with public_key = Even_PC.to_pc sequencer_key }
            in
            List.iter [ inner_account; holder_account; sequencer_account ]
              ~f:(fun acc ->
                L.create_new_account_exn ledger
                  (Account_id.create acc.public_key acc.token_id)
                  acc ) ;
            let tids =
              Account_id.derive_token_id ~owner:Zeko_constants.inner_account_id
              :: Account_id.derive_token_id
                   ~owner:
                     ( Account_id.of_public_key
                     @@ Public_key.decompress_exn holder_account.public_key )
              :: Account_id.derive_token_id
                   ~owner:
                     ( Account_id.of_public_key
                     @@ Public_key.decompress_exn sequencer_account.public_key
                     )
              ::
              ( match faucet_aid with
              | None ->
                  []
              | Some faucet_aid ->
                  L.create_new_account_exn ledger faucet_aid
                    (Account.create faucet_aid Currency.Balance.max_int) ;
                  [ Account_id.derive_token_id ~owner:faucet_aid ] )
            in
            let imt_hash =
              printf "Creating imt\n%!" ;
              let imt, _witnesses =
                Indexed_merkle_tree.Db.create_of_entries_exn
                  ~depth:constraint_constants.ledger_depth tids
              in
              Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
            in
            (None, ledger, imt_hash)
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

            let accounts_diff =
              (0, inner_account) :: (1, holder_account)
              ::
              ( match faucet_aid with
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
            let imt_hash =
              printf "Creating imt\n%!" ;
              let tids =
                L.to_list_sequential ledger
                |> List.map ~f:Account.identifier
                |> List.map ~f:(fun aid ->
                       Account_id.derive_token_id ~owner:aid )
              in
              let imt, _witnesses =
                Indexed_merkle_tree.Db.create_of_entries_exn
                  ~depth:constraint_constants.ledger_depth tids
              in
              let imt_hash = Indexed_merkle_tree.Db.merkle_root imt in
              printf "IMT hash: %s\n%!" (Ledger_hash.to_decimal_string imt_hash) ;
              Account_set.of_fields [| imt_hash |]
            in
            ( Some (old_ledger_hash, old_ledger_openings, accounts_diff)
            , ledger
            , imt_hash )
      in
      let%bind command =
        Sequencer_lib.Deploy.deploy_command_exn
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
          ~signer:sender_keypair ~outer_kp ~holder_kp ~token_holder_kp
          ~fee:(Currency.Fee.of_mina_int_exn 1)
          ~nonce ~account_creation_fee ~initial_ledger:new_ledger
          ~account_set_hash:imt_hash ~pause_key ~sequencer:sequencer_key ~da_key
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
        if List.length da_nodes = 0 || old_and_new_ledger_same then return ()
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
            Da_layer.Diff.create
              ~source_ledger_hash:
                (Sparse_ledger.merkle_root old_ledger_openings)
              ~changed_accounts ~command_with_action_step_flags:None
          in
          Da_layer.Client.distribute_diff ~logger ~config:da_config
            ~ledger_openings:old_ledger_openings ~diff
        else
          let () =
            print_endline
              "(* Post the whole genesis diff with all the accounts *)"
          in
          Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
            ~ledger:new_ledger
      in

      print_endline "(* Deploy contract *)" ;
      match%bind
        Sequencer_lib.Gql_client.send_zkapp l1_uri
          (Zkapp_command.read_all_proofs_from_disk command)
      with
      | Ok _ ->
          Deferred.unit
      | Error (`Failed_request err) ->
          eprintf "Failed request: %s\n%!" err ;
          Deferred.unit
      | Error (`Graphql_error err) ->
          eprintf "Graphql error: %s\n%!" err ;
          Deferred.unit )

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Deploy zeko zkapp"
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
        and da_key = flag "--da-key" (required string) ~doc:"string Da key"
        and account_creation_fee =
          flag "--account-creation-fee" (required string)
            ~doc:"float Account creation fee in mina"
        in
        let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
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
        let da_key =
          string_to_even_pc da_key |> Option.value_exn ~message:"DA key odd"
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
        let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
          Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
        in
        run ~l1_uri ~sk ~ledger_input ~faucet_aid ~da_nodes ~pause_key
          ~sequencer_key ~da_key ~account_creation_fee )
