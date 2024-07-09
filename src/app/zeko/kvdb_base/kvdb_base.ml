open Core_kernel

module Make (Key : sig
  type t

  val serialize : t -> Bigstring.t
end) =
struct
  include Mina_ledger.Ledger.Kvdb

  let set t ~key ~data = set t ~key:(Key.serialize key) ~data

  let get t ~key = get t ~key:(Key.serialize key)
end
