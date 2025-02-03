open Core
open Mina_base
open Async
open Mina_ledger
open Signature_lib
module L = Ledger

let constraint_constants = Genesis_constants.Compiled.constraint_constants

module Test_accounts = struct
  type t = { pk : string; balance : int64 } [@@deriving yojson]

  let parse_accounts_exn ~test_accounts_path : (Account_id.t * Account.t) list =
    let accounts =
      Yojson.Safe.(
        from_file test_accounts_path
        |> Util.to_list
        |> List.map ~f:(fun t ->
               match of_yojson t with
               | Ppx_deriving_yojson_runtime.Result.Ok t ->
                   t
               | Ppx_deriving_yojson_runtime.Result.Error e ->
                   failwith e ))
    in
    List.map accounts ~f:(fun { pk; balance } ->
        let account_id =
          Account_id.create
            (Public_key.Compressed.of_base58_check_exn pk)
            Token_id.default
        in
        let account =
          Account.create account_id
            (Currency.Balance.of_uint64 (Unsigned.UInt64.of_int64 balance))
        in
        (account_id, account) )
end

let run ~l1_uri ~sk ~initial_state ~da_nodes ~pause_key ~sequencer_key ~da_key
    () =
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
      let ledger, imt_hash =
        let ledger =
          L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
        in
        L.create_new_account_exn ledger Zeko_constants.inner_account_id
          initial_inner_account ;
        match initial_state with
        | `None ->
            ( ledger
            , Indexed_merkle_tree.Sparse_indexed_merkle_tree.(
                create_from_tids ~depth:constraint_constants.ledger_depth []
                |> merkle_root) )
        | `Test_accounts test_accounts_path ->
            let accounts =
              Test_accounts.parse_accounts_exn ~test_accounts_path
            in
            let tids =
              List.map accounts ~f:(fun (aid, _) ->
                  Account_id.derive_token_id ~owner:aid )
            in
            let imt_hash =
              Indexed_merkle_tree.Sparse_indexed_merkle_tree.(
                create_from_tids ~depth:constraint_constants.ledger_depth tids
                |> merkle_root)
            in
            let ledger =
              List.fold ~init:ledger accounts
                ~f:(fun ledger (account_id, account) ->
                  L.create_new_account_exn ledger account_id account ;
                  ledger )
            in
            (ledger, imt_hash)
        | `Db_dir (db_dir, imt_dir) ->
            let imt_hash =
              Indexed_merkle_tree.Db.(
                create ~directory_name:imt_dir
                  ~depth:constraint_constants.ledger_depth ()
                |> merkle_root)
            in
            let db =
              L.of_database
              @@ L.Db.create ~directory_name:db_dir
                   ~depth:constraint_constants.ledger_depth ()
            in
            (db, imt_hash)
      in
      let%bind command =
        Sequencer_lib.Deploy.deploy_command_exn ~signer:sender_keypair
          ~zkapp:zkapp_keypair
          ~fee:(Currency.Fee.of_mina_int_exn 1)
          ~nonce ~constraint_constants ~initial_ledger:ledger
          ~account_set_hash:imt_hash ~pause_key ~sequencer:sequencer_key ~da_key
      in

      (* Post genesis batch *)
      let%bind () =
        let config = Da_layer.Client.Config.{ nodes = da_nodes } in
        match%bind
          Da_layer.Client.distribute_genesis_diff ~logger ~config ~ledger
        with
        | Ok _ ->
            return ()
        | Error e ->
            Error.raise e
      in

      (* Deploy contract *)
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
        and test_accounts_path =
          flag "--test-accounts-path" (optional string)
            ~doc:"string Path to the test genesis accounts file"
        and init_db_dir =
          flag "--init-db-dir" (optional string)
            ~doc:"string Path to the initial db"
        and init_imt_dir =
          flag "--init-imt-dir" (optional string)
            ~doc:"string Path to the initial imt"
        and da_nodes =
          flag "--da-node" (listed string)
            ~doc:"string Address of the DA node, can be supplied multiple times"
        and pause_key =
          flag "--pause-key" (required string) ~doc:"string Pause key"
        and sequencer_key =
          flag "--sequencer-key" (required string) ~doc:"string Sequencer key"
        and da_key = flag "--da-key" (required string) ~doc:"string Da key" in
        let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
        let da_nodes =
          List.mapi da_nodes ~f:(fun i uri ->
              Cli_lib.Flag.Types.
                { value = Host_and_port.of_string uri
                ; name = sprintf "da-node-%d" i
                } )
        in
        let initial_state =
          match (test_accounts_path, init_db_dir, init_imt_dir) with
          | Some _, Some _, _ | Some _, _, Some _ ->
              failwith "Cannot specify both test accounts and initial db"
          | None, Some _, None | None, None, Some _ ->
              failwith "Cannot specify only one of db and imt"
          | Some test_accounts_path, None, None ->
              `Test_accounts test_accounts_path
          | None, Some init_db_dir, Some init_imt_dir ->
              `Db_dir (init_db_dir, init_imt_dir)
          | None, None, None ->
              `None
        in

        let string_to_even_pc x =
          Public_key.Compressed.of_base58_check_exn x
          |> Zeko_circuits.Zeko_util.Even_PC.create_exn
        in
        let pause_key = string_to_even_pc pause_key in
        let sequencer_key = string_to_even_pc sequencer_key in
        let da_key = string_to_even_pc da_key in

        let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
          Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
        in
        run ~l1_uri ~sk ~initial_state ~da_nodes ~pause_key ~sequencer_key
          ~da_key )
