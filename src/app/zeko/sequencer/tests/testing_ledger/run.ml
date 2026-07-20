open Core
open Async
open Mina_ledger
open Mina_base
open Cli_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let run ~logger ~port ~db_dir ~genesis_account ~block_period ~network_id
    ~disable_proofs =
  let create_state () =
    let () =
      let open Core in
      match Unix.system ("rm -rf " ^ db_dir) with
      | Ok () ->
          ()
      | Error (`Exit_non_zero result) ->
          [%log error] "Failed to remove db_dir: %s" db_dir ;
          exit result
      | Error (`Signal signal) ->
          [%log error] "Failed to remove db_dir: %s, signal: %s" db_dir
            (Signal.to_string signal) ;
          exit 1
    in
    let t =
      State.create ~logger
        ~signature_kind:(Utils.signature_kind network_id)
        ~disable_proofs ~db_dir
        ~block_period:
          (Option.map block_period
             ~f:(Fn.compose Time_ns.Span.of_sec Int.to_float) )
        ()
    in
    ( if Option.is_some genesis_account then
      let account_id =
        Account_id.create
          (Signature_lib.Public_key.Compressed.of_base58_check_exn
             (Option.value_exn genesis_account) )
          Token_id.default
      in
      let account =
        Account.create account_id
          (Currency.Balance.of_uint64
             (Unsigned.UInt64.of_int64 1_000_000_000_000L) )
      in
      ( Ledger.Db.get_or_create_account t.db account_id account
        : ([ `Added | `Existed ] * Ledger.Db.Location.t) Or_error.t )
      |> ignore ) ;
    t
  in
  let t = ref @@ create_state () in
  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req -> !t)
      (Gql.schema ~reset_callback:(fun () ->
           State.shutdown !t ;
           t := create_state () ) )
  in
  let () =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            [%log error] "Unhandled exception: %s" (Exn.to_string exn) ) )
      (Tcp.Where_to_listen.bind_to Tcp.Bind_to_address.Localhost
         (Tcp.Bind_to_port.On_port port) )
      (fun ~body _sock req ->
        let headers = Cohttp.Request.headers req in
        match Cohttp.Header.get headers "Connection" with
        | Some "Upgrade" ->
            Graphql_cohttp_async.respond_string ~status:`Forbidden
              ~body:"Websocket not supported" ()
        | _ ->
            graphql_callback () req body )
    |> Deferred.ignore_m |> don't_wait_for
  in
  [%log info] "Local network listening on port %d" port ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Local network"
    (let%map_open.Command log_json = Flag.Log.json
     and log_level = Flag.Log.level
     and port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and genesis_account =
       flag "--genesis-account" (optional string)
         ~doc:"string Optional public key of genesis account"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "l1_db" string)
         ~doc:"string Directory to store the database"
     and block_period =
       flag "--block-period" (optional int)
         ~doc:"int Optional block period in seconds"
     and network_id =
       flag "--network-id"
         (optional_with_default "testnet" string)
         ~doc:"string Network id"
     and disable_proofs =
       flag "--disable-proofs" no_arg ~doc:"bool Disable proofs"
     in
     let logger = Logger.create () in
     Stdout_log.setup log_json log_level ;
     run ~logger ~port ~db_dir ~genesis_account ~block_period ~network_id
       ~disable_proofs )
  |> Command_unix.run
