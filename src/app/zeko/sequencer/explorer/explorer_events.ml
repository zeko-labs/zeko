(* Defines the shared explorer-facing NATS subjects, payload encoding, and
   publishing helpers used by both the live sequencer path and backfill jobs. *)

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
