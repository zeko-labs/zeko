open Core_kernel
open Mina_base

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { source_ledger_hash : Ledger_hash.Stable.V1.t
      ; diff : (int * Account.Stable.V2.t) list
      }
    [@@deriving yojson, fields]

    let to_latest = Fn.id
  end
end]

let to_bigstring = Binable.to_bigstring (module Stable.Latest)

let of_bigstring = Binable.of_bigstring (module Stable.Latest)
