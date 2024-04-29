open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger
module Ws = Websocket.Make (Cohttp_async.Io)

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let compile_time_genesis_state =
  let genesis_constants = Genesis_constants.compiled in
  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol
  in
  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
  in
  compile_time_genesis.data

module Graphql_ws = struct
  type client_message =
    | Gql_connection_init
    | Gql_start of
        { id : int
        ; query : string
        ; variables : Yojson.Safe.t
        ; operation_name : string option
        }

  type server_message =
    | Gql_connection_ack
    | Gql_data of { data : Yojson.Safe.t }
    | Gql_unknown of string

  let client_message_to_string =
    Fn.compose Yojson.Safe.to_string (function
      | Gql_connection_init ->
          `Assoc [ ("type", `String "connection_init"); ("payload", `Assoc []) ]
      | Gql_start { id; query; variables; operation_name } ->
          `Assoc
            [ ("type", `String "start")
            ; ("id", `String (Int.to_string id))
            ; ( "payload"
              , `Assoc
                  [ ("query", `String query)
                  ; ("variables", variables)
                  ; ( "operationName"
                    , Option.value ~default:`Null
                      @@ Option.map ~f:(fun s -> `String s) operation_name )
                  ] )
            ] )

  let server_message_of_string s =
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string s in
    let type_ = json |> member "type" |> to_string in
    match type_ with
    | "connection_ack" ->
        Gql_connection_ack
    | "data" ->
        Gql_data { data = json |> member "payload" |> member "data" }
    | _ ->
        Gql_unknown s
end

module Bootstrap = struct
  let fetch_committed_history ~zeko_uri () =
    let query =
      {|
        query {
          committedTransactions {
            rawTransactions
            jsonGenesisAccounts {
              account
              accountId
            }
          }
        }
      |}
    in
    let body =
      Yojson.Safe.to_string
      @@ `Assoc [ ("query", `String query); ("variables", `Assoc []) ]
    in
    let headers =
      List.fold ~init:(Cohttp.Header.init ())
        ~f:(fun acc (k, v) -> Cohttp.Header.add acc k v)
        [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
    in
    let%bind.Deferred.Result response, body =
      Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true (fun () ->
          Cohttp_async.Client.post ~headers
            ~body:(Cohttp_async.Body.of_string body)
            zeko_uri )
      |> Deferred.Result.map_error ~f:(fun e -> Error.to_string_hum e)
    in
    let%bind body_str = Cohttp_async.Body.to_string body in
    let%bind.Deferred.Result body_json =
      match
        Cohttp.Code.code_of_status (Cohttp_async.Response.status response)
      with
      | 200 ->
          Deferred.return (Ok (Yojson.Safe.from_string body_str))
      | code ->
          Deferred.return
            (Error (Printf.sprintf "Status code %d -- %s" code body_str))
    in
    let open Yojson.Safe.Util in
    match (member "errors" body_json, member "data" body_json) with
    | `Null, `Null ->
        return (Error "Empty response from graphql query")
    | error, `Null ->
        return (Error (Yojson.Safe.to_string error))
    | _, raw_json ->
        let raw_transactions =
          member "committedTransactions" raw_json
          |> member "rawTransactions" |> to_list |> List.map ~f:to_string
        in
        let genesis_accounts =
          member "committedTransactions" raw_json
          |> member "jsonGenesisAccounts"
          |> to_list
          |> List.map ~f:(fun account_json ->
                 let ok_exn = function Ok x -> x | Error e -> failwith e in
                 let filter_string_escape =
                   Fn.compose Yojson.Safe.from_string Yojson.Safe.Util.to_string
                 in
                 let account =
                   member "account" account_json
                   |> filter_string_escape |> Account.of_yojson |> ok_exn
                 in
                 let account_id =
                   member "accountId" account_json
                   |> filter_string_escape |> Account_id.of_yojson |> ok_exn
                 in
                 (account_id, account) )
        in
        return
          (Ok
             ( genesis_accounts
             , List.map raw_transactions
                 ~f:(Fn.compose Or_error.ok_exn User_command.of_base64) ) )

  let bootstrap ~logger ~zeko_uri ~archive_uri () =
    Thread_safe.block_on_async_exn (fun () ->
        let%bind genesis_accounts, commands =
          match%bind fetch_committed_history ~zeko_uri () with
          | Ok history ->
              return history
          | Error e ->
              failwith e
        in
        Ledger.with_ledger ~depth:constraint_constants.ledger_depth
          ~f:(fun ledger ->
            List.iter genesis_accounts ~f:(fun (account_id, account) ->
                print_endline "Creating account" ;
                Ledger.create_new_account_exn ledger account_id account ) ;

            let protocol_state = ref compile_time_genesis_state in
            Deferred.List.iter commands ~f:(fun command ->
                let txn_applied =
                  Or_error.ok_exn
                  @@ Result.( >>= )
                       (Ledger.apply_transaction_first_pass
                          ~constraint_constants
                          ~global_slot:
                            Mina_numbers.Global_slot_since_genesis.zero
                          ~txn_state_view:
                            Mina_state.Protocol_state.(
                              Body.view @@ body compile_time_genesis_state)
                          ledger (Command command) )
                       (Ledger.apply_transaction_second_pass ledger)
                in
                Ledger.commit ledger ;
                let new_protocol_state, diff =
                  Archive_lib.Diff.Builder.transaction_added
                    ~constraint_constants
                    ~accounts_created:
                      (Ledger.Transaction_applied.new_accounts txn_applied)
                    ~new_state_hash:(Ledger.merkle_root ledger)
                    ~protocol_state:!protocol_state ~ledger
                    ~txn:(Ledger.Transaction_applied.transaction txn_applied)
                in
                protocol_state := new_protocol_state ;
                match%bind
                  Archive_client.dispatch ~logger archive_uri
                    (Archive_lib.Diff.Transition_frontier diff)
                with
                | Ok () ->
                    return @@ print_endline "Bootrapped transaction to archive"
                | Error e ->
                    raise (Error.to_exn e) ) ) )
end

let handshake r w =
  let%bind () =
    Pipe.write w (Graphql_ws.client_message_to_string Gql_connection_init)
  in
  match%bind Pipe.read r with
  | `Ok message -> (
      match Graphql_ws.server_message_of_string message with
      | Gql_connection_ack ->
          return ()
      | _ ->
          failwith "connection_ack not received" )
  | `Eof ->
      failwith "eof"

let subscribe w =
  let subscribe_payload =
    Graphql_ws.client_message_to_string
      (Gql_start
         { id = 1
         ; query = "subscription { newTransaction }"
         ; variables = `Assoc []
         ; operation_name = None
         } )
  in
  Pipe.write w subscribe_payload

let relay_txn_to_archive_exn ~archive_uri ~logger data =
  let raw =
    Base64.decode_exn
    @@ Yojson.Safe.Util.(member "newTransaction" data |> to_string)
  in
  let diff =
    Binable.of_string (module Archive_lib.Diff.Transition_frontier) raw
  in
  match%bind
    Archive_client.dispatch ~logger archive_uri
      (Archive_lib.Diff.Transition_frontier diff)
  with
  | Ok () ->
      return @@ print_endline "Relayed transaction to archive"
  | Error e ->
      raise (Error.to_exn e)

let rec run ~logger ~zeko_uri ~archive_uri () =
  let () =
    match
      Thread_safe.block_on_async (fun () ->
          Conduit_async.V3.with_connection_uri zeko_uri (fun _ r w ->
              print_endline "Connected to zeko" ;
              let r, w =
                Websocket_async.client_ez ~heartbeat:(Time_ns.Span.of_sec 15.)
                  zeko_uri r w
              in

              let%bind () = handshake r w in
              let%bind () = subscribe w in

              Pipe.iter r ~f:(fun message ->
                  match Graphql_ws.server_message_of_string message with
                  | Gql_data { data } -> (
                      match%bind
                        try_with (fun () ->
                            relay_txn_to_archive_exn ~archive_uri ~logger data )
                      with
                      | Ok () ->
                          return ()
                      | Error e ->
                          return @@ print_endline (Exn.to_string e) )
                  | Gql_unknown s ->
                      return @@ print_endline ("Received unknown message: " ^ s)
                  | _ ->
                      return () ) ) )
    with
    | Ok () ->
        print_endline "Disconnected gracefully"
    | Error e ->
        print_endline (Exn.to_string e)
  in
  print_endline "Retrying in 5 seconds" ;
  Thread_safe.block_on_async_exn (fun () -> after (Time.Span.of_sec 5.)) ;
  run ~logger ~zeko_uri ~archive_uri ()

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run archive adapter for zeko"
       (let%map_open.Command zeko_uri =
          flag "--zeko-uri" (required string) ~doc:"Zeko sequencer graphql uri"
        and archive_host =
          flag "--archive-host" (required string) ~doc:"Archive node host"
        and archive_port =
          flag "--archive-port" (required int) ~doc:"Archive node port"
        and bootstrap =
          flag "--bootstrap" no_arg
            ~doc:
              "Bootstrap the archive from the da layer, requires DA_PROVIDER \
               env var"
        in
        let logger = Logger.create () in
        let zeko_uri = Uri.of_string zeko_uri in
        let archive_uri =
          Cli_lib.Flag.Types.
            { value = Host_and_port.create ~host:archive_host ~port:archive_port
            ; name = "archive-uri"
            }
        in

        if bootstrap then Bootstrap.bootstrap ~logger ~zeko_uri ~archive_uri () ;

        run ~logger ~zeko_uri ~archive_uri )
