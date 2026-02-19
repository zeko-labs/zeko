open Core_kernel

module Database : Merkle_ledger.Intf.Key_value_database with type config := string =
struct
  module Bigstring_frozen = struct
    module T = struct
      include Bigstring.Stable.V1

      let hash = hash_t_frozen

      let hash_fold_t = hash_fold_t_frozen
    end

    include T
    include Hashable.Make_binable (T)
  end

  type t =
    { uuid : Uuid.Stable.V1.t
    ; table : Bigstring_frozen.t Bigstring_frozen.Table.t
    }
  [@@deriving sexp]

  let copy_bigstring t : Bigstring.t =
    let len = Bigstring.length t in
    let copied = Bigstring.create len in
    Bigstring.blit ~src:t ~src_pos:0 ~dst:copied ~dst_pos:0 ~len ;
    copied

  let to_alist t =
    Bigstring_frozen.Table.to_alist t.table
    |> List.sort
         ~compare:(fun (k1, _) (k2, _) -> Bigstring_frozen.compare k1 k2)
    |> List.map ~f:(fun (k, v) -> (copy_bigstring k, copy_bigstring v))

  let get_uuid t = t.uuid

  let create (_ : string) =
    { uuid = Uuid.create_random Random.State.default
    ; table = Bigstring_frozen.Table.create ()
    }

  let create_checkpoint t (_ : string) =
    { uuid = Uuid.create_random Random.State.default
    ; table = Bigstring_frozen.Table.copy t.table
    }

  let close _ = ()

  let get t ~key =
    Bigstring_frozen.Table.find t.table key |> Option.map ~f:copy_bigstring

  let get_batch t ~keys = List.map keys ~f:(fun key -> get t ~key)

  let set t ~key ~data =
    Bigstring_frozen.Table.set t.table ~key:(copy_bigstring key)
      ~data:(copy_bigstring data)

  let set_batch t ?(remove_keys = []) ~key_data_pairs =
    List.iter key_data_pairs ~f:(fun (key, data) -> set t ~key ~data) ;
    List.iter remove_keys ~f:(fun key -> Bigstring_frozen.Table.remove t.table key)

  let remove t ~key = Bigstring_frozen.Table.remove t.table key

  let make_checkpoint _ _ = ()

  let foldi t ~init ~f =
    List.foldi (to_alist t) ~init ~f:(fun i acc (key, data) -> f i acc ~key ~data)

  let fold_until t ~init ~f ~finish =
    let step acc (key, data) = f acc ~key ~data in
    List.fold_until (to_alist t) ~init ~f:step ~finish

  let zeko_prev_key t ~key =
    let rec loop prev = function
      | [] -> (
          match prev with
          | Some k ->
              k
          | None ->
              failwith "zeko_prev_key: no previous key found" )
      | (k, _) :: rest ->
          if Bigstring.compare k key >= 0 then (
            match prev with
            | Some k ->
                k
            | None ->
                failwith "zeko_prev_key: no previous key found" )
          else loop (Some k) rest
    in
    loop None (to_alist t)
end
