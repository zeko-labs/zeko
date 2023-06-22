open Core_kernel

module Monad = struct
  module type S = sig
    type 'a t

    include Monad.S with type 'a t := 'a t

    module Result : sig
      val lift : 'value t -> ('value, 'err) Result.t t

      type nonrec ('value, 'err) t = ('value, 'err) Result.t t

      include Monad.S2 with type ('value, 'err) t := ('value, 'err) t
    end

    module Option : sig
      type nonrec 'a t = 'a option t

      include Monad.S with type 'a t := 'a t
    end
  end

  module Ident = struct
    include Monad.Ident

    module Result = struct
      let lift = Result.return

      include Result
    end

    module Option = Option
  end
end

module type S = sig
  type t [@@deriving sexp]

  type key

  type value

  type config

  val create : config -> t

  val close : t -> unit

  val get : t -> key:key -> value option

  val get_batch : t -> keys:key list -> value option list

  val set : t -> key:key -> data:value -> unit

  val remove : t -> key:key -> unit

  val set_batch :
    t -> ?remove_keys:key list -> key_data_pairs:(key * value) list -> unit

  val to_alist : t -> (key * value) list

  val create_checkpoint : t -> string -> t

  val make_checkpoint : t -> string -> unit

  val get_uuid : t -> Uuid.t

  (* an association list, sorted by key *)
end

module Make_mock
    (Key : sig
      type t
      include Hashable.S with type t := t
      val sexp_of_t : t -> Sexp.t
    end) (Value : sig
      type t [@@deriving sexp]
    end) :
  S
    with type t = Value.t Key.Table.t
     and type key := Key.t
     and type value := Value.t
     and type config := string = struct
  type t = Value.t Key.Table.t

  let sexp_of_t t =
    Key.Table.to_alist t
    |> List.map ~f:(fun (key, value) ->
           [%sexp_of: Sexp.t * Sexp.t] (Key.sexp_of_t key, Value.sexp_of_t value) )
    |> [%sexp_of: Sexp.t list]

  let t_of_sexp _ = raise (Failure "unimplemented mock t_of_sexp")

  let create _ = Key.Table.create ()

  let get t ~key = Key.Table.find t key

  let get_batch t ~keys = List.map keys ~f:(Key.Table.find t)

  let set = Key.Table.set

  let remove t ~key = Key.Table.remove t key

  let close _ = ()

  let set_batch t ?(remove_keys = []) ~key_data_pairs =
    List.iter key_data_pairs ~f:(fun (key, data) -> set t ~key ~data) ;
    List.iter remove_keys ~f:(fun key -> remove t ~key)

  let to_alist = Key.Table.to_alist

  let get_uuid _ = raise (Failure "no mock UUID")

  let create_checkpoint _ _ = raise (Failure "unimplemented mock create_checkpoint")
  let make_checkpoint _ _ = raise (Failure "unimplemented mock make_checkpoint")
end
