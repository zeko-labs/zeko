[@@@warning "-67"]

open Snark_params.Tick
open Zeko_util

module With_length : sig
  type tag_var

  type tag_t

  val tag :
    ( tag_var
    , tag_t
    , Compile_simple.self_width
    , Folder.tag_branches )
    Pickles.Tag.t
    lazy_t

  type stmt = { action_state : field; length : Checked32.t }

  type t = { source : stmt; target : stmt; proof : Proof.t }

  val leaf : stmt -> field list -> t Promise.t

  val leaf_iterations : int

  val leaf_option : stmt -> field list -> t Promise.t

  val leaf_option_iterations : int

  val extend : t -> field list -> t Promise.t

  val extend_iterations : int

  val extend_option : t -> field list -> t Promise.t

  val extend_option_iterations : int

  val merge : t -> t -> t Promise.t

  module Make (Inputs : sig
    module Action_state : Rollup_state.Action_state_type

    (** Set this as high as possible. Makes your circuit bigger. *)
    val get_iterations : int
  end) : sig
    open Inputs

    module Stmt : sig
      type t =
        { source : Action_state.With_length.t
        ; target : Action_state.With_length.t
        }
      [@@deriving snarky]
    end

    include SnarkType

    (** You should pass the second result into previous_proof_statements *)
    val get :
         ?check:Boolean.var
           (** Set this to false if you don't want to check the proof after all. *)
      -> var
      -> (Stmt.var * (tag_var, Compile_simple.self_width) Compile_simple.prev)
         Checked.t
  end
end

module Without_length : sig
  type tag_var

  type tag_t

  val tag :
    ( tag_var
    , tag_t
    , Compile_simple.self_width
    , Folder.tag_branches )
    Pickles.Tag.t
    lazy_t

  type t = { source : field; target : field; proof : Proof.t }

  val leaf : field -> field list -> t Promise.t

  val leaf_iterations : int

  val leaf_option : field -> field list -> t Promise.t

  val leaf_option_iterations : int

  val extend : t -> field list -> t Promise.t

  val extend_iterations : int

  val extend_option : t -> field list -> t Promise.t

  val extend_option_iterations : int

  val merge : t -> t -> t Promise.t

  module Make (Inputs : sig
    module Action_state : Rollup_state.Action_state_type

    (** Set this as high as possible. Makes your circuit bigger. *)
    val get_iterations : int
  end) : sig
    open Inputs

    module Stmt : sig
      type t = { source : Action_state.t; target : Action_state.t }
      [@@deriving snarky]
    end

    include SnarkType

    (** You should pass the second result into previous_proof_statements *)
    val get :
         ?check:Boolean.var
           (** Set this to false if you don't want to check the proof after all. *)
      -> var
      -> (Stmt.var * (tag_var, Compile_simple.self_width) Compile_simple.prev)
         Checked.t
  end
end
