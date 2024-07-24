open Core
open Mina_base
open Async
open Mina_ledger
module L = Ledger

let constraint_constants = Genesis_constants.Constraint_constants.compiled

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
            (Signature_lib.Public_key.Compressed.of_base58_check_exn pk)
            Token_id.default
        in
        let account =
          Account.create account_id
            (Currency.Balance.of_uint64 (Unsigned.UInt64.of_int64 balance))
        in
        (account_id, account) )
end

module T = Transaction_snark.Make (struct
  let constraint_constants = constraint_constants

  let proof_level = Genesis_constants.Proof_level.Full
end)

module M = Zkapps_rollup.Make (T)

let run ~uri ~sk ~initial_state ~da_nodes () =
  let logger = Logger.create () in
  let sender_keypair =
    Signature_lib.(
      Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk)
  in
  let zkapp_keypair = Signature_lib.Keypair.create () in
  printf "zkapp secret key: %s\n%!"
    (Signature_lib.Private_key.to_base58_check zkapp_keypair.private_key) ;
  printf "zkapp public key: %s\n%!"
    Signature_lib.Public_key.(
      Compressed.to_base58_check @@ compress zkapp_keypair.public_key) ;

  let nonce =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer_lib.Gql_client.infer_nonce uri
          (Signature_lib.Public_key.compress sender_keypair.public_key) )
  in

  let ledger =
    let ledger =
      L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
    in
    L.create_new_account_exn ledger M.Inner.account_id M.Inner.initial_account ;
    match initial_state with
    | `None ->
        ledger
    | `Test_accounts test_accounts_path ->
        List.fold ~init:ledger
          (Test_accounts.parse_accounts_exn ~test_accounts_path)
          ~f:(fun ledger (account_id, account) ->
            L.create_new_account_exn ledger account_id account ;
            ledger )
    | `Db_dir db_dir ->
        L.of_database
        @@ L.Db.create ~directory_name:db_dir
             ~depth:constraint_constants.ledger_depth ()
  in
  let command =
    Sequencer_lib.Deploy.deploy_command_exn ~signer:sender_keypair
      ~zkapp:zkapp_keypair
      ~fee:(Currency.Fee.of_mina_int_exn 1)
      ~nonce ~constraint_constants ~initial_ledger:ledger
      (module M)
  in

  (* Post genesis batch *)
  Thread_safe.block_on_async_exn (fun () ->
      let config = Da_layer.Client.Config.{ nodes = da_nodes } in
      match%bind
        Da_layer.Client.distribute_genesis_batch ~logger ~config ~ledger
      with
      | Ok _ ->
          return ()
      | Error e ->
          Error.raise e ) ;

  (* Deploy contract *)
  Thread_safe.block_on_async_exn (fun () ->
      match%bind Sequencer_lib.Gql_client.send_zkapp uri command with
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
       (let%map_open.Command uri = Cli_lib.Flag.Uri.Client.rest_graphql
        and test_accounts_path =
          flag "--test-accounts-path" (optional string)
            ~doc:"string Path to the test genesis accounts file"
        and init_db_dir =
          flag "--init-db-dir" (optional string)
            ~doc:"string Path to the initial db"
        and da_nodes =
          flag "--da-node" (listed string)
            ~doc:"string Address of the DA node, can be supplied multiple times"
        in
        let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
        let da_nodes =
          List.mapi da_nodes ~f:(fun i uri ->
              Cli_lib.Flag.Types.
                { value = Host_and_port.of_string uri
                ; name = sprintf "da-node-%d" i
                } )
        in
        let initial_state =
          match (test_accounts_path, init_db_dir) with
          | Some _, Some _ ->
              failwith "Cannot specify both test accounts and initial db"
          | Some test_accounts_path, None ->
              `Test_accounts test_accounts_path
          | None, Some init_db_dir ->
              `Db_dir init_db_dir
          | None, None ->
              `None
        in
        run ~uri ~sk ~initial_state ~da_nodes )
