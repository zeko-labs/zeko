open Core_kernel
open Mina_base
open Mina_ledger

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
            (** Source ledger hash of the diff *)
      ; diff : (int * Account.Stable.V2.t) list
            (** List of changed accounts with corresponding index in the ledger *)
      ; command_with_action_step_flags :
          (User_command.Stable.V2.t * bool list) option
            (** Optionally add command with corresponding action steps to store the history *)
      }
    [@@deriving yojson, fields]

    let to_latest = Fn.id
  end
end]

let create ~source_ledger_hash ~diff ~command_with_action_step_flags =
  { source_ledger_hash; diff; command_with_action_step_flags }

let to_bigstring = Binable.to_bigstring (module Stable.Latest)

let of_bigstring = Binable.of_bigstring (module Stable.Latest)

(** [Ledger_hash.empty_hash] is [zero], so we need this for the genesis state of the rollup *)
let empty_ledger_hash ~depth =
  Ledger.merkle_root @@ Ledger.create_ephemeral ~depth ()
