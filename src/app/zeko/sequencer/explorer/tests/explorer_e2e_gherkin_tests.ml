(* Runs slow Gherkin-backed explorer E2E scenarios against the same external
   services used by the sequencer integration test: L1 test ledger, DA nodes,
   signers, RabbitMQ, Postgres, and a real NATS broker. *)

open Core
open Async
open Mina_base
open Sequencer_lib
open Zeko_sequencer
open Test_spec
open Handle.Operator

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Spam ;
  Logger.create ()

let run = Thread_safe.block_on_async_exn

let gql_uri = Uri.of_string "http://localhost:8080/graphql"

let nats_url () =
  match Sys.getenv "NATS_URL" with
  | Some value ->
      Uri.of_string value
  | None ->
      failwith "NATS_URL must be set for explorer E2E Gherkin tests"

let da_config =
  Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555"; "127.0.0.1:8556" ]

let da_keys =
  run (fun () -> Da_layer.Client.Config.fetch_public_keys ~logger da_config)

let da_quorum = 2

let mq_host = Host_and_port.of_string "localhost:5672"

let slot_acceptance = Time.Span.of_min 60.

let postgres_uri () =
  run (fun () ->
      Relational_db.For_tests.create_database ~port:5433
        "explorer_e2e_gherkin" )

let drop_postgres () =
  run (fun () ->
      Relational_db.For_tests.drop_database ~port:5433
        "explorer_e2e_gherkin" )

let shutdown_sequencer sequencer =
  Gc.full_major () ;
  run (fun () -> Sequencer.shutdown !sequencer) ;
  Handle.invalidate sequencer

let connect_or_fail uri =
  Clock_ns.with_timeout (Time_ns.Span.of_sec 5.)
    (Nats_client_async.connect (Some uri))
  >>= function
  | `Timeout ->
      failwithf "timed out connecting to %s" (Uri.to_string uri) ()
  | `Result client ->
      return client

let expect_ok label = function
  | Ok value ->
      value
  | Error err ->
      failwithf "%s failed: %s" label (Error.to_string_hum err) ()

let subscribe_transactions subscriber =
  let%map subscription =
    Nats_client_async.subscribe subscriber
      ~subject:Explorer_events.Subject.transactions ()
  in
  expect_ok "transaction subscription" subscription

let wait_for_subscription_registration () =
  Clock_ns.after (Time_ns.Span.of_ms 100.)

let read_message label reader =
  Clock_ns.with_timeout (Time_ns.Span.of_sec 10.) (Pipe.read reader)
  >>= function
  | `Timeout ->
      failwithf "%s timed out waiting for a message" label ()
  | `Result `Eof ->
      failwithf "%s closed before delivering a message" label ()
  | `Result (`Ok message) ->
      return message

let read_until label reader ~f =
  let rec go remaining =
    if remaining <= 0
    then failwithf "%s did not observe the expected message" label () ;
    let%bind message = read_message label reader in
    match f message with
    | Some value ->
        return value
    | None ->
        go (remaining - 1)
  in
  go 20

let headers message =
  message.headers
  |> Option.value ~default:Nats_client.Headers.empty
  |> Nats_client.Headers.to_list

let require_header message ~name ~expected =
  match List.Assoc.find (headers message) ~equal:String.equal name with
  | Some value when String.equal value expected ->
      ()
  | Some value ->
      failwithf "expected header %s=%s but received %s" name expected value ()
  | None ->
      failwithf "missing header %s" name ()

let json_assoc key json =
  match json with
  | `Assoc fields ->
      List.Assoc.find fields key ~equal:String.equal
  | _ ->
      None

let json_assoc_exn key json =
  Option.value_exn (json_assoc key json)
    ~message:(sprintf "missing JSON field %s" key)

let message_payload message = Yojson.Safe.from_string message.payload

let assert_safe_json_equal actual expected =
  if not (Yojson.Safe.equal actual expected)
  then
    failwithf "Expected %s but got %s" (Yojson.Safe.to_string expected)
      (Yojson.Safe.to_string actual) ()

let target_hash_json target_ledger_hash =
  Ledger_hash.to_yojson target_ledger_hash

let target_hash_string target_ledger_hash =
  Ledger_hash.to_decimal_string target_ledger_hash

let create_sequencer_spec ~postgres_uri =
  Quickcheck.random_value
    (Sequencer_spec.gen ~logger ~number_of_transactions:1 ~postgres_uri
       ~gql_uri ~da_config ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       ~nats_url:(nats_url ()) () )

let apply_first_transaction sequencer specs =
  let spec = List.hd_exn specs in
  let command =
    User_command.Signed_command
      (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec)
  in
  run (fun () ->
      Sequencer.apply_user_command !sequencer command >>| Or_error.ok_exn) ;
  Sequencer.get_root !sequencer

let assert_user_command_message message ~target_ledger_hash =
  if
    not
      (String.equal message.subject Explorer_events.Subject.transactions)
  then
    failwithf "unexpected subject %s" message.subject () ;
  require_header message ~name:"Nats-Msg-Id"
    ~expected:(target_hash_string target_ledger_hash) ;
  let payload = message_payload message in
  assert_safe_json_equal (json_assoc_exn "kind" payload)
    (`String "user_command") ;
  assert_safe_json_equal (json_assoc_exn "target_ledger_hash" payload)
    (target_hash_json target_ledger_hash)

let wait_for_backfill_completion job =
  let rec go remaining =
    if remaining <= 0 then failwith "backfill job did not complete" ;
    if Explorer_backfill_service.is_terminal job.status
    then return ()
    else Clock_ns.after (Time_ns.Span.of_ms 200.) >>= fun () -> go (remaining - 1)
  in
  go 100

let run_scenarios () =
  Feature_parser.assert_scenario "e2e.feature"
    "Sequencer-applied transactions are published to NATS" ;
  Feature_parser.assert_scenario "e2e.feature"
    "Backfill replays sequencer DA diffs to NATS" ;
  let postgres_uri = postgres_uri () in
  let nats_uri = nats_url () in
  let subscriber = ref None in
  let sequencer = ref None in
  let backfill_service = ref None in
  Exn.protect
    ~finally:(fun () ->
      Option.iter !backfill_service ~f:(fun service ->
          run (fun () -> Explorer_backfill_service.shutdown service)) ;
      Option.iter !sequencer ~f:(fun sequencer ->
          ignore (shutdown_sequencer sequencer : (_, Handle.invalid) Handle.t)) ;
      Option.iter !subscriber ~f:(fun client ->
          run (fun () -> Nats_client_async.close client)) ;
      drop_postgres ())
    ~f:(fun () ->
      let client = run (fun () -> connect_or_fail nats_uri) in
      subscriber := Some client ;
      let live_subscription =
        run (fun () ->
            let%bind subscription = subscribe_transactions client in
            let%map () = wait_for_subscription_registration () in
            subscription)
      in
      let { Sequencer_spec.sequencer = sequencer_handle; specs; _ } =
        create_sequencer_spec ~postgres_uri
      in
      sequencer := Some sequencer_handle ;
      let target_ledger_hash = apply_first_transaction sequencer_handle specs in
      let live_message =
        run (fun () ->
            read_message "sequencer publish" live_subscription.messages)
      in
      assert_user_command_message live_message ~target_ledger_hash ;
      let backfill_subscription =
        run (fun () ->
            let%bind subscription = subscribe_transactions client in
            let%map () = wait_for_subscription_registration () in
            subscription)
      in
      let service =
        run (fun () ->
            Explorer_backfill_service.create ~logger ~da_config
              ~nats_url:nats_uri)
      in
      backfill_service := Some service ;
      let job =
        Explorer_backfill_service.start_backfill service
          ~from_hash:Explorer_backfill_service.genesis_hash
          ~to_hash:target_ledger_hash
      in
      run (fun () -> wait_for_backfill_completion job) ;
      let replay_payload =
        run (fun () ->
            read_until "backfill replay" backfill_subscription.messages
              ~f:(fun message ->
                let payload = message_payload message in
                match json_assoc "target_ledger_hash" payload with
                | Some target
                  when Yojson.Safe.equal target
                         (target_hash_json target_ledger_hash) ->
                    require_header message ~name:"Nats-Msg-Id"
                      ~expected:(target_hash_string target_ledger_hash) ;
                    Some payload
                | _ ->
                    None))
      in
      assert_safe_json_equal (json_assoc_exn "kind" replay_payload)
        (`String "sync_replay") ;
      printf "explorer E2E Gherkin scenarios passed against %s\n"
        (Uri.to_string nats_uri))

let () = run_scenarios ()
