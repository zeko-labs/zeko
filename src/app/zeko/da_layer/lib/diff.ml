open Core_kernel
open Mina_base
open Mina_ledger
open Snark_params.Tick

module Actions = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        [ `Command_with_action_step_flags of
          User_command.Stable.V2.t * bool list
        | `Actions of
          (Account_id.Stable.V2.t * (Field.t[@version_asserted]) list list list)
          list ]
      [@@deriving yojson, sexp, compare]

      let to_latest = Fn.id
    end
  end]

  let of_legacy_command_with_action_step_flags = function
    | Some (command, flags) ->
        `Command_with_action_step_flags (command, flags)
    | None ->
        `Actions []
end

[%%versioned
module Stable = struct
  [@@@with_top_version_tag]

  [@@@no_toplevel_latest_type]

  module V4 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; changed_accounts : (int * Account.Stable.V2.t) list
      ; actions : Actions.Stable.V1.t
      ; timestamp : Block_time.Stable.V1.t
      ; acc_set : (Field.t[@version_asserted])
      }
    [@@deriving yojson, fields, sexp_of, compare]

    let to_latest = Fn.id
  end

  module V3 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; changed_accounts : (int * Account.Stable.V2.t) list
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
      ; timestamp : Block_time.Stable.V1.t
      ; acc_set : (Field.t[@version_asserted])
      }
    [@@deriving yojson, fields, sexp_of, compare]

    let to_latest t =
      { V4.source_ledger_hash = t.source_ledger_hash
      ; changed_accounts = t.changed_accounts
      ; actions =
          Actions.of_legacy_command_with_action_step_flags
            t.command_with_action_step_flags
      ; timestamp = t.timestamp
      ; acc_set = t.acc_set
      }
  end

  module V2 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; changed_accounts : (int * Account.Stable.V2.t) list
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
      ; timestamp : Block_time.Stable.V1.t
      }
    [@@deriving yojson, fields, sexp_of, compare]

    let to_latest t =
      { V4.source_ledger_hash = t.source_ledger_hash
      ; changed_accounts = t.changed_accounts
      ; actions =
          Actions.of_legacy_command_with_action_step_flags
            t.command_with_action_step_flags
      ; timestamp = t.timestamp
      ; acc_set = Field.zero
      }
  end

  module V1 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; changed_accounts : (int * Account.Stable.V2.t) list
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
      }
    [@@deriving yojson, fields, sexp]

    let to_latest ?(timestamp = Block_time.zero) ?(acc_set = Field.zero) (t : t)
        =
      { V4.source_ledger_hash = t.source_ledger_hash
      ; changed_accounts = t.changed_accounts
      ; actions =
          Actions.of_legacy_command_with_action_step_flags
            t.command_with_action_step_flags
      ; timestamp
      ; acc_set
      }
  end
end]

module Pending = struct
  [%%versioned
  module Stable = struct
    [@@@with_top_version_tag]

    module V1 = struct
      type t =
        { source_ledger_hash : Ledger_hash.Stable.V1.t
        ; changed_accounts : (int * Account.Stable.V2.t) list
        ; actions : Actions.Stable.V1.t
        }
      [@@deriving yojson, fields, sexp]

      let to_latest = Fn.id
    end
  end]
end

type t = Stable.V4.t =
  { source_ledger_hash : Ledger_hash.t
  ; changed_accounts : (int * Account.t) list
  ; actions : Actions.t
  ; timestamp : Block_time.t
  ; acc_set : (Field.t[@version_asserted])
  }
[@@deriving to_yojson, fields, sexp_of]

let create_pending ~source_ledger_hash ~changed_accounts ~actions =
  { Pending.source_ledger_hash; changed_accounts; actions }

let add_time_and_acc_set ~logger t ~acc_set =
  { Stable.V4.source_ledger_hash = t.Pending.source_ledger_hash
  ; changed_accounts = t.changed_accounts
  ; actions = t.actions
  ; timestamp = Block_time.now (Block_time.Controller.basic ~logger)
  ; acc_set
  }

let drop_time
    { Stable.V4.source_ledger_hash
    ; changed_accounts
    ; actions
    ; acc_set = _
    ; timestamp = _
    } =
  { Pending.source_ledger_hash; changed_accounts; actions }

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
  let v4 = of_bigstring v1_serialized |> Or_error.ok_exn in

  [%test_eq: Stable.V4.t] (Stable.V1.to_latest v1) (Stable.V4.to_latest v4)
