(* Covers the explorer event payload contract and message id generation in a
   dedicated test module instead of mixing tests into the implementation file. *)

open Core_kernel
open Mina_base

let find_assoc_exn json key =
  match json with
  | `Assoc fields ->
      List.Assoc.find_exn fields key ~equal:String.equal
  | _ ->
      failwith "Expected JSON object"

let%test_unit "transaction payload encoding keeps contract fields" =
  let target_ledger_hash = Ledger_hash.empty_hash in
  let diff =
    Explorer_events.build_live_diff ~logger:(Logger.create ())
      ~diff:
        (Da_layer.Diff.create ~source_ledger_hash:Ledger_hash.empty_hash
           ~changed_accounts:[] ~command_with_action_step_flags:None )
      ~acc_set_root:Snark_params.Tick.Field.zero
  in
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.User_command
      ~target_ledger_hash ~genesis:false ~diff
  in
  [%test_eq: string] message.subject Explorer_events.Subject.transactions ;
  [%test_eq: (string * string) list] message.headers
    (Explorer_events.nats_msg_id_headers target_ledger_hash) ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "kind")
    (`String "user_command") ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "genesis")
    (`Bool false)

let%test_unit "finality payload encoding keeps hashes and status" =
  let message =
    Explorer_events.build_finality_message ~logger:(Logger.create ())
      ~status:Explorer_events.Finality_status.Committed
      ~source_ledger_hash:Ledger_hash.empty_hash
      ~target_ledger_hash:Ledger_hash.empty_hash
  in
  [%test_eq: string] message.subject Explorer_events.Subject.finality ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "status")
    (`String "committed")

let%test_unit "health payload encoding includes component and instance id" =
  let message =
    Explorer_events.build_health_message ~logger:(Logger.create ())
      ~component:"sequencer" ~instance_id:"instance-1" ~status:"ok"
  in
  [%test_eq: string] message.subject Explorer_events.Subject.health ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "component")
    (`String "sequencer") ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "instance_id")
    (`String "instance-1")

let%test_unit "nats msg id uses target ledger hash" =
  [%test_eq: string] (Explorer_events.nats_msg_id Ledger_hash.empty_hash)
    (Ledger_hash.to_decimal_string Ledger_hash.empty_hash)
