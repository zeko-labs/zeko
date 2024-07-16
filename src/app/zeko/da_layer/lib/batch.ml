open Core_kernel
open Mina_base
open Mina_ledger

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; target_ledger_hash : Ledger_hash.Stable.V1.t
      ; diff : (int * Account.Stable.V2.t) list
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
      }
    [@@deriving yojson, fields]

    let to_latest = Fn.id
  end
end]

let create ~source_ledger_hash ~target_ledger_hash ~diff
    ~command_with_action_step_flags =
  { source_ledger_hash
  ; target_ledger_hash
  ; diff
  ; command_with_action_step_flags
  }

let empty =
  create ~source_ledger_hash:Ledger_hash.empty_hash
    ~target_ledger_hash:Ledger_hash.empty_hash ~diff:[]
    ~command_with_action_step_flags:None

let to_bigstring = Binable.to_bigstring (module Stable.Latest)

let of_bigstring = Binable.of_bigstring (module Stable.Latest)

let empty_ledger_hash ~depth =
  Ledger.merkle_root @@ Ledger.create_ephemeral ~depth ()
