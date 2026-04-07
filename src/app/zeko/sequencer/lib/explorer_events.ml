open Core_kernel
open Mina_base

module Transaction_kind = struct
  type t =
    | User_command
    | Fee_transfer
    | Sync_replay
    | Genesis_replay

  let to_string = function
    | User_command ->
        "user_command"
    | Fee_transfer ->
        "fee_transfer"
    | Sync_replay ->
        "sync_replay"
    | Genesis_replay ->
        "genesis_replay"
end

module Finality_status = struct
  type t =
    | Proved
    | Committed

  let to_string = function Proved -> "proved" | Committed -> "committed"
end

module Subject = struct
  let transactions = "zeko.l2.transactions"
  let finality = "zeko.l2.finality"
  let health = "zeko.health"
end

type message =
  { subject : string
  ; headers : (string * string) list
  ; payload : Yojson.Safe.t
  }

type sink = message -> unit

let noop_sink _ = ()

let timestamp_json ~logger =
  Block_time.now (Block_time.Controller.basic ~logger) |> Block_time.to_yojson

let nats_msg_id target_ledger_hash =
  Ledger_hash.to_decimal_string target_ledger_hash

let nats_msg_id_headers target_ledger_hash =
  [ ("Nats-Msg-Id", nats_msg_id target_ledger_hash) ]

let create_nats_sink client : sink =
 fun { subject; headers; payload } ->
  let headers = Nats_client.Headers.of_list headers in
  Nats_client_async.publish_json client ~subject ~headers payload

let build_transaction_message ~kind ~target_ledger_hash ~genesis ~diff =
  { subject = Subject.transactions
  ; headers = nats_msg_id_headers target_ledger_hash
  ; payload =
      `Assoc
        [ ("kind", `String (Transaction_kind.to_string kind))
        ; ("target_ledger_hash", Ledger_hash.to_yojson target_ledger_hash)
        ; ("genesis", `Bool genesis)
        ; ("diff", Da_layer.Diff.Stable.V3.to_yojson diff)
        ]
  }

let build_live_diff ~logger ~diff ~acc_set_root =
  Da_layer.Diff.add_time_and_acc_set ~logger diff ~acc_set:acc_set_root

let build_finality_message ~logger ~status ~source_ledger_hash
    ~target_ledger_hash =
  { subject = Subject.finality
  ; headers = []
  ; payload =
      `Assoc
        [ ("status", `String (Finality_status.to_string status))
        ; ("source_ledger_hash", Ledger_hash.to_yojson source_ledger_hash)
        ; ("target_ledger_hash", Ledger_hash.to_yojson target_ledger_hash)
        ; ("timestamp", timestamp_json ~logger)
        ]
  }

let build_health_message ~logger ~component ~instance_id ~status =
  { subject = Subject.health
  ; headers = []
  ; payload =
      `Assoc
        [ ("component", `String component)
        ; ("instance_id", `String instance_id)
        ; ("status", `String status)
        ; ("timestamp", timestamp_json ~logger)
        ]
  }

let publish sink message = sink message

let publish_transaction sink ~kind ~target_ledger_hash ~genesis ~diff =
  publish sink
  @@ build_transaction_message ~kind ~target_ledger_hash ~genesis ~diff

let publish_finality sink ~logger ~status ~source_ledger_hash ~target_ledger_hash
    =
  publish sink
  @@ build_finality_message ~logger ~status ~source_ledger_hash
       ~target_ledger_hash

let publish_health sink ~logger ~component ~instance_id ~status =
  publish sink @@ build_health_message ~logger ~component ~instance_id ~status

let find_assoc_exn json key =
  match json with
  | `Assoc fields ->
      List.Assoc.find_exn fields key ~equal:String.equal
  | _ ->
      failwith "Expected JSON object"

let%test_unit "transaction payload encoding keeps contract fields" =
  let target_ledger_hash = Ledger_hash.empty_hash in
  let diff =
    build_live_diff ~logger:(Logger.create ())
      ~diff:
        (Da_layer.Diff.create ~source_ledger_hash:Ledger_hash.empty_hash
           ~changed_accounts:[] ~command_with_action_step_flags:None )
      ~acc_set_root:Snark_params.Tick.Field.zero
  in
  let message =
    build_transaction_message ~kind:Transaction_kind.User_command
      ~target_ledger_hash ~genesis:false ~diff
  in
  [%test_eq: string] message.subject Subject.transactions ;
  [%test_eq: (string * string) list] message.headers
    (nats_msg_id_headers target_ledger_hash) ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "kind")
    (`String "user_command") ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "genesis")
    (`Bool false)

let%test_unit "finality payload encoding keeps hashes and status" =
  let message =
    build_finality_message ~logger:(Logger.create ())
      ~status:Finality_status.Committed
      ~source_ledger_hash:Ledger_hash.empty_hash
      ~target_ledger_hash:Ledger_hash.empty_hash
  in
  [%test_eq: string] message.subject Subject.finality ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "status")
    (`String "committed")

let%test_unit "health payload encoding includes component and instance id" =
  let message =
    build_health_message ~logger:(Logger.create ()) ~component:"sequencer"
      ~instance_id:"instance-1" ~status:"ok"
  in
  [%test_eq: string] message.subject Subject.health ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "component")
    (`String "sequencer") ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn message.payload "instance_id")
    (`String "instance-1")

let%test_unit "nats msg id uses target ledger hash" =
  [%test_eq: string] (nats_msg_id Ledger_hash.empty_hash)
    (Ledger_hash.to_decimal_string Ledger_hash.empty_hash)
