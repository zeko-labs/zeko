open Core
open Mina_base
open Async
open Mina_ledger
open Signature_lib
open Zeko_types
module L = Ledger

let constraint_constants = Zeko_constants.constraint_constants

let print_endline = Core.print_endline

let run ~l1_uri ~sk ~ledger_input ~faucet_account ~da_nodes ~pause_key
    ~sequencer_key ~da_key ~network ~account_creation_fee () =
  let logger = Logger.create () in
  let sender_keypair =
    Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk
  in
  let zkapp_keypair = Keypair.create () in
  printf "zkapp secret key: %s\n%!"
    (Private_key.to_base58_check zkapp_keypair.private_key) ;
  printf "zkapp public key: %s\n%!"
    Public_key.(Compressed.to_base58_check @@ compress zkapp_keypair.public_key) ;

  Thread_safe.block_on_async_exn (fun () ->
      let%bind nonce =
        Sequencer_lib.Gql_client.infer_nonce l1_uri
          (Public_key.compress sender_keypair.public_key)
      in
      let%bind initial_inner_account =
        Sequencer_lib.Deploy.Z.Inner.initial_account ()
      in
      let old_ledger_witness, new_ledger, imt_hash =
        let ledger =
          L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
        in
        match ledger_input with
        | None ->
            L.create_new_account_exn ledger Zeko_constants.inner_account_id
              initial_inner_account ;
            let () =
              match faucet_account with
              | None ->
                  ()
              | Some faucet_account ->
                  let aid =
                    Account_id.of_public_key
                      Public_key.(
                        decompress_exn
                        @@ Compressed.of_base58_check_exn faucet_account)
                  in
                  L.create_new_account_exn ledger aid
                    (Account.create aid Currency.Balance.max_int)
            in
            ( None
            , ledger
            , Account_set.of_fields
                [| Indexed_merkle_tree.Db.(
                     create ~depth:constraint_constants.ledger_depth ()
                     |> merkle_root)
                |] )
        | Some ledger_input_json ->
            print_endline "(* Load ledger from json file *)" ;
            Yojson.Safe.from_file ledger_input_json
            |> Yojson.Safe.Util.to_list
            |> List.map ~f:[%of_yojson: int * Account.t]
            |> List.map ~f:(function Ok x -> x | Error e -> failwith e)
            |> List.iter ~f:(fun (index, account) ->
                   L.set_at_index_exn ledger index account ) ;
            let old_ledger_hash = L.merkle_root ledger in
            let old_inner_account_opening =
              Sparse_ledger.of_ledger_subset_exn ledger
                [ Zeko_constants.inner_account_id ]
            in

            print_endline "(* Overwrite inner account *)" ;
            L.set_at_index_exn ledger 0 initial_inner_account ;

            print_endline "(* Construct IMT *)" ;
            let imt_hash =
              printf "Creating imt\n%!" ;
              let imt =
                Indexed_merkle_tree.Db.create
                  ~depth:constraint_constants.ledger_depth ()
              in
              let tids =
                L.to_list_sequential ledger
                |> List.map ~f:Account.identifier
                |> List.map ~f:(fun aid ->
                       Account_id.derive_token_id ~owner:aid )
              in
              List.iter tids ~f:(fun tid ->
                  let _witness =
                    Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
                  in
                  () ) ;
              Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
            in
            (Some (old_ledger_hash, old_inner_account_opening), ledger, imt_hash)
      in
      let%bind command =
        Sequencer_lib.Deploy.deploy_command_exn ~signature_kind:network
          ~signer:sender_keypair ~zkapp:zkapp_keypair
          ~fee:(Currency.Fee.of_mina_int_exn 1)
          ~nonce ~account_creation_fee ~initial_ledger:new_ledger
          ~account_set_hash:imt_hash ~pause_key ~sequencer:sequencer_key ~da_key
          ()
      in

      let da_config = Da_layer.Client.Config.{ nodes = da_nodes } in

      (* If the old ledger exists, we need to just post the diff with updated inner account *)
      let old_ledger_hash = Option.map old_ledger_witness ~f:fst in
      let%bind old_ledger_exists =
        match old_ledger_witness with
        | Some (ledger_hash, _) ->
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
          let old_inner_account_opening =
            Option.(value_exn @@ map old_ledger_witness ~f:snd)
          in
          let diff =
            Da_layer.Diff.create
              ~source_ledger_hash:
                (Sparse_ledger.merkle_root old_inner_account_opening)
              ~changed_accounts:[ (0, initial_inner_account) ]
              ~command_with_action_step_flags:None
          in
          Da_layer.Client.distribute_diff ~logger ~config:da_config
            ~ledger_openings:old_inner_account_opening ~diff
        else
          let () =
            print_endline
              "(* Post the whole genesis diff with all the accounts *)"
          in
          Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
            ~ledger:new_ledger
      in

      print_endline "(* Deploy contract *)" ;
      match%bind Sequencer_lib.Gql_client.send_zkapp l1_uri command with
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
        and network = flag "--network" (optional string) ~doc:"string Network"
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
        let network =
          match network with
          | None | Some "testnet" ->
              Mina_signature_kind.Testnet
          | Some "mainnet" ->
              Mainnet
          | Some network ->
              Other_network network
        in
        let account_creation_fee =
          Currency.Fee.of_mina_string_exn account_creation_fee
        in
        let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
          Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
        in
        let () =
          match (faucet_account, ledger_input) with
          | Some _, Some _ ->
              failwith
                "Faucet account and ledger input cannot be provided together"
          | _ ->
              ()
        in
        run ~l1_uri ~sk ~ledger_input ~faucet_account ~da_nodes ~pause_key
          ~sequencer_key ~da_key ~network ~account_creation_fee )
