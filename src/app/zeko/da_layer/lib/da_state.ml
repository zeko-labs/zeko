open Core_kernel
open Mina_base
open Mina_ledger
module Field = Snark_params.Tick.Field

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t =
      { ledger_hash : Ledger_hash.Stable.V1.t
      ; acc_set : (Field.t[@version_asserted])
      }
    [@@deriving compare, equal, sexp, yojson]

    let to_latest = Fn.id
  end
end]

type t = Stable.V1.t = { ledger_hash : Ledger_hash.t; acc_set : Field.t }
[@@deriving compare, equal, sexp, yojson]

let create ~ledger_hash ~acc_set = { ledger_hash; acc_set }

let to_string { ledger_hash; acc_set } =
  Ledger_hash.to_decimal_string ledger_hash ^ ":" ^ Field.to_string acc_set

let of_string value =
  match String.rsplit2 value ~on:':' with
  | Some (ledger_hash, acc_set) ->
      Or_error.try_with (fun () ->
          { ledger_hash = Ledger_hash.of_decimal_string ledger_hash
          ; acc_set = Field.of_string acc_set
          } )
  | None ->
      Or_error.errorf "Invalid DA state: %s" value

let signing_message { ledger_hash; acc_set } =
  Random_oracle.hash
    ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
    [| ledger_hash; acc_set |]

let empty ~depth =
  let acc_set = Indexed_merkle_tree.In_memory.create ~depth () in
  { ledger_hash = Ledger.merkle_root @@ Ledger.create_ephemeral ~depth ()
  ; acc_set = Indexed_merkle_tree.In_memory.merkle_root acc_set
  }

let%test_unit "account-set root is part of the DA signing message" =
  let ledger_hash = Ledger_hash.empty_hash in
  let first = signing_message (create ~ledger_hash ~acc_set:Field.zero) in
  let second = signing_message (create ~ledger_hash ~acc_set:Field.one) in
  assert (not (Field.equal first second))
