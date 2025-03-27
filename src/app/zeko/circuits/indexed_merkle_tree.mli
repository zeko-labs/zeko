open Snark_params.Tick
open Zeko_util

(** We have a sparse merkle tree, where at each leaf we store the Entry.
    At the beginning, our merkle tree has each leaf set to 0, for which
    we assume no preimage exists, except two otherwise unspecial entries,
    which most be respectively the maximum and minimum value.

    When adding a key, we simultaneously ensure that it did not exist before,
    by finding a key lesser than our own for which the next key is greater than our own.
    If our logic is implemented correctly this should only happen if no key between these exists
    in the merkle tree.
    We then add a new entry at a previously empty location in the merkle tree,
    at the specified height.
    An empty location is a leaf which has the "hash image" of 0.

    We need the witness that is the path to the old lesser key,
    and where the new key is to be.
  *)
module Make (Inputs : sig
  module Key : SnarkType

  val zero_var : Key.var

  val assert_x_less_than_y_less_than_z :
    x:Key.var -> y:Key.var -> z:Key.var -> unit Checked.t

  val height : int
end) : sig
  open Inputs

  type t = F.t [@@deriving sexp]

  type var

  val typ : (var, t) Typ.t

  module PathStep : sig
    type t = { hash_other : F.t; is_right : Boolean.t } [@@deriving snarky]
  end

  module Path : sig
    type t = PathStep.t list

    type var = PathStep.var list

    val typ : (var, t) Typ.t
  end

  val add_key_var :
       ?check:Boolean.var
    -> x:Key.var
    -> path_x:Path.var
    -> y:Key.var
    -> path_y:Path.var
    -> z:Key.var
    -> unit
    -> ([ `Before_adding_y of var ] * [ `After_adding_y of var ]) Checked.t
end
