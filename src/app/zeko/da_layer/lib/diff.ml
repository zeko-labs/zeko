open Core_kernel
open Mina_base
open Mina_ledger

[%%versioned
module Stable = struct
  [@@@with_top_version_tag]

  module V2 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
            (** Source ledger hash of the diff *)
      ; changed_accounts : (int * Account.Stable.V2.t) list
            (** List of changed accounts with corresponding index in the ledger *)
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
            (** Optionally add command with corresponding action steps to store the history *)
      ; timestamp : Block_time.Stable.V1.t  (** Timestamp of the diff *)
      }
    [@@deriving yojson, fields, sexp_of, compare]

    let to_latest = Fn.id
  end

  module V1 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
            (** Source ledger hash of the diff *)
      ; changed_accounts : (int * Account.Stable.V2.t) list
            (** List of changed accounts with corresponding index in the ledger *)
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
            (** Optionally add command with corresponding action steps to store the history *)
      }
    [@@deriving yojson, fields, sexp]

    let to_latest ?(timestamp = Block_time.zero) (t : t) =
      { V2.source_ledger_hash = t.source_ledger_hash
      ; changed_accounts = t.changed_accounts
      ; command_with_action_step_flags = t.command_with_action_step_flags
      ; timestamp
      }
  end
end]

let create ~source_ledger_hash ~changed_accounts ~command_with_action_step_flags
    =
  { Stable.V1.source_ledger_hash
  ; changed_accounts
  ; command_with_action_step_flags
  }

let changed_accounts { Stable.V1.changed_accounts; _ } = changed_accounts

let source_ledger_hash { Stable.V1.source_ledger_hash; _ } = source_ledger_hash

let command_with_action_step_flags
    { Stable.V1.command_with_action_step_flags; _ } =
  command_with_action_step_flags

let add_time ~logger t =
  Stable.V1.to_latest
    ~timestamp:(Block_time.now (Block_time.Controller.basic ~logger))
    t

let drop_time
    { Stable.V2.source_ledger_hash
    ; changed_accounts
    ; command_with_action_step_flags
    ; _
    } =
  { Stable.V1.source_ledger_hash
  ; changed_accounts
  ; command_with_action_step_flags
  }

let to_bigstring =
  Binable.to_bigstring (module Stable.Latest.With_top_version_tag)

let of_bigstring bigstring =
  let pos_ref = ref 0 in
  Stable.bin_read_top_tagged_to_latest ~pos_ref bigstring

(** [Ledger_hash.empty_hash] is [zero], so we need this for the genesis state of the rollup *)
let empty_ledger_hash ~depth =
  Ledger.merkle_root @@ Ledger.create_ephemeral ~depth ()

let%test_unit "diff versioning" =
  let v1 =
    Stable.V1.
      { source_ledger_hash = Ledger_hash.empty_hash
      ; changed_accounts = []
      ; command_with_action_step_flags = None
      }
  in
  let v1_serialized =
    Binable.to_bigstring (module Stable.V1.With_top_version_tag) v1
  in
  let v2 = of_bigstring v1_serialized |> Or_error.ok_exn in

  [%test_eq: Stable.V2.t] (Stable.V1.to_latest v1) (Stable.V2.to_latest v2)
