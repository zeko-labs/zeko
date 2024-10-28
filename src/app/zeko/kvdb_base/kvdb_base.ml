open Core_kernel

(** A key-value database interface
    GADT ensures that keys and values are correctly matched *)
module Make (Key_value : sig
  type _ t

  val serialize_key : ('k * 'v) t -> 'k -> Bigstring.t

  val serialize_value : ('k * 'v) t -> 'v -> Bigstring.t

  val deserialize_value : ('k * 'v) t -> Bigstring.t -> 'v
end) =
struct
  include Mina_ledger.Ledger.Kvdb

  let set t pair_type ~key ~data =
    set t
      ~key:(Key_value.serialize_key pair_type key)
      ~data:(Key_value.serialize_value pair_type data)

  let get t pair_type ~key =
    get t ~key:(Key_value.serialize_key pair_type key)
    |> Option.map ~f:(Key_value.deserialize_value pair_type)
end
