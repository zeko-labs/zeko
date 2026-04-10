(* Executes Gherkin-backed explorer integration scenarios against a real NATS
   server supplied through the NATS_URL environment variable. *)

open Core
open Async
open Mina_base

let url () =
  match Sys.getenv "NATS_URL" with
  | Some value ->
      Uri.of_string value
  | None ->
      failwith "NATS_URL must be set for explorer NATS integration tests"

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

let read_message label reader =
  Clock_ns.with_timeout (Time_ns.Span.of_sec 5.) (Pipe.read reader)
  >>= function
  | `Timeout ->
      failwithf "%s timed out waiting for a message" label ()
  | `Result `Eof ->
      failwithf "%s closed before delivering a message" label ()
  | `Result (`Ok message) ->
      return message

let wait_for_subscription_registration () =
  (* The async NATS client exposes subscribe but not a flush/ack primitive,
     so give the server a brief moment to register the SUB before publishing. *)
  Clock_ns.after (Time_ns.Span.of_ms 100.)

let require_header headers ~name ~expected =
  match List.Assoc.find headers ~equal:String.equal name with
  | Some value when String.equal value expected ->
      ()
  | Some value ->
      failwithf "expected header %s=%s but received %s" name expected value ()
  | None ->
      failwithf "missing header %s" name ()

let json_assoc_exn key json =
  match json with
  | `Assoc fields ->
      List.Assoc.find_exn fields key ~equal:String.equal
  | _ ->
      failwith "Expected JSON object"

let assert_safe_json_equal actual expected =
  if not (Yojson.Safe.equal actual expected)
  then
    failwithf "Expected %s but got %s" (Yojson.Safe.to_string expected)
      (Yojson.Safe.to_string actual) ()

let sample_diff ?(source_ledger_hash = Ledger_hash.empty_hash) () =
  Explorer_events.build_live_diff ~logger:(Logger.create ())
    ~diff:
      (Da_layer.Diff.create ~source_ledger_hash ~changed_accounts:[]
         ~command_with_action_step_flags:None )
    ~acc_set_root:Snark_params.Tick.Field.zero

let test_service nats_client : Explorer_backfill_service.t =
  { logger = Logger.create ()
  ; da_config = Da_layer.Client.Config.of_string_list []
  ; nats_client = Some nats_client
  ; jobs = String.Table.create ()
  ; instance_id = "integration-instance"
  ; started_at = Time.epoch
  }

let with_clients f =
  let uri = url () in
  let%bind subscriber = connect_or_fail uri in
  let%bind actor = connect_or_fail uri in
  Monitor.protect
    ~finally:(fun () ->
      Nats_client_async.close actor >>= fun () ->
      Nats_client_async.close subscriber)
    (fun () -> f ~subscriber ~actor)

let run_live_transaction_scenario () =
  Feature_parser.assert_scenario "nats-integration.feature"
    "Live transaction events round-trip through NATS with the dedup header" ;
  with_clients (fun ~subscriber ~actor ->
      let%bind subscription =
        Nats_client_async.subscribe subscriber
          ~subject:Explorer_events.Subject.transactions ()
      in
      let subscription = expect_ok "live transaction subscription" subscription in
      let%bind () = wait_for_subscription_registration () in
      let target_ledger_hash = Ledger_hash.empty_hash in
      let sink = Explorer_events.create_nats_sink actor in
      Explorer_events.publish_transaction sink
        ~kind:Explorer_events.Transaction_kind.User_command
        ~target_ledger_hash ~genesis:false ~diff:(sample_diff ()) ;
      let%map message =
        read_message "live transaction publish" subscription.messages
      in
      if not (String.equal message.subject Explorer_events.Subject.transactions)
      then
        failwithf "unexpected transaction subject: %s" message.subject () ;
      let headers =
        message.headers
        |> Option.value ~default:Nats_client.Headers.empty
        |> Nats_client.Headers.to_list
      in
      require_header headers ~name:"Nats-Msg-Id"
        ~expected:(Ledger_hash.to_decimal_string target_ledger_hash) ;
      let payload = Yojson.Safe.from_string message.payload in
      assert_safe_json_equal (json_assoc_exn "kind" payload)
        (`String "user_command") )

let run_backfill_scenario () =
  Feature_parser.assert_scenario "nats-integration.feature"
    "Backfill replay events round-trip through NATS as genesis replays" ;
  with_clients (fun ~subscriber ~actor ->
      let%bind subscription =
        Nats_client_async.subscribe subscriber
          ~subject:Explorer_events.Subject.transactions ()
      in
      let subscription = expect_ok "backfill subscription" subscription in
      let%bind () = wait_for_subscription_registration () in
      let service = test_service actor in
      let target_ledger_hash = Ledger_hash.empty_hash in
      let publish_result =
        Explorer_backfill_service.publish_backfill_diff service
          ~from_hash:Explorer_backfill_service.genesis_hash ~index:0
          ~target_ledger_hash
          (sample_diff ~source_ledger_hash:Explorer_backfill_service.genesis_hash
             () )
      in
      if not (Poly.equal publish_result `Queued)
      then failwith "expected backfill publish to be queued" ;
      let%map message =
        read_message "backfill publish" subscription.messages
      in
      if not (String.equal message.subject Explorer_events.Subject.transactions)
      then
        failwithf "unexpected backfill subject: %s" message.subject () ;
      let payload = Yojson.Safe.from_string message.payload in
      assert_safe_json_equal (json_assoc_exn "kind" payload)
        (`String "genesis_replay") ;
      assert_safe_json_equal (json_assoc_exn "genesis" payload) (`Bool true) )

let main () =
  let%bind () = run_live_transaction_scenario () in
  let%bind () = run_backfill_scenario () in
  printf "explorer NATS Gherkin integration scenarios passed against %s\n"
    (Uri.to_string (url ())) ;
  return ()

let () = Thread_safe.block_on_async_exn main
