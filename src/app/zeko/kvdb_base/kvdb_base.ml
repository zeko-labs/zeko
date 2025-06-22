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

  let set_raw = set

  let set t pair_type ~key ~data =
    set_raw t
      ~key:(Key_value.serialize_key pair_type key)
      ~data:(Key_value.serialize_value pair_type data)

  type packed = Pack : ('k * 'v) Key_value.t * 'k * 'v -> packed

  type packed_key = Pack_key : ('k * 'v) Key_value.t * 'k -> packed_key

  let set_batch t ?(remove_keys : packed_key list option) (batch : packed list)
      =
    let raw_batch =
      List.map batch ~f:(fun (Pack (pair_type, key, data)) ->
          ( Key_value.serialize_key pair_type key
          , Key_value.serialize_value pair_type data ) )
    in
    let remove_keys =
      Option.map remove_keys
        ~f:
          (List.map ~f:(fun (Pack_key (pair_type, key)) ->
               Key_value.serialize_key pair_type key ) )
    in
    set_batch ?remove_keys t ~key_data_pairs:raw_batch

  let get_raw = get

  let get t pair_type ~key =
    get_raw t ~key:(Key_value.serialize_key pair_type key)
    |> Option.map ~f:(Key_value.deserialize_value pair_type)
end

module Make_singleton (Inputs : sig
  type t [@@deriving yojson]

  val key : string
end) =
struct
  module Key_value = struct
    type _ t = Singleton : (unit * Inputs.t) t

    let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
     fun pair_type _ ->
      match pair_type with Singleton -> Bigstring.of_string Inputs.key

    let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
     fun pair_type value ->
      match pair_type with
      | Singleton ->
          Bigstring.of_string @@ Yojson.Safe.to_string @@ Inputs.to_yojson value

    let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
      let ok_exn x =
        let open Ppx_deriving_yojson_runtime.Result in
        match x with Ok x -> x | Error e -> failwith e
      in
      fun pair_type data ->
        match pair_type with
        | Singleton ->
            ok_exn @@ Inputs.of_yojson @@ Yojson.Safe.from_string
            @@ Bigstring.to_string data
  end

  include Make (Key_value)

  let set t = set t Singleton ~key:()

  let get t = get t Singleton ~key:()
end

type t = Mina_ledger.Ledger.Kvdb.t
