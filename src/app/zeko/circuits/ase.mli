[@@@warning "-67"]

open Snark_params.Tick
open Zeko_util

module M_with_length : sig
  module Stmt : sig
    type t = { action_state : F.t; length : Checked32.t } [@@deriving snarky]
  end

  module Init = Stmt

  val init : check:'a -> Init.var -> Stmt.var Checked.t

  val step : field_var -> Stmt.var -> Stmt.var Checked.t
end

module With_length : sig
  module Stmt = M_with_length.Stmt

  type trans = { source : Stmt.t; target : Stmt.t }

  val leaf : (field list * Stmt.t -> (trans * Proof.t) Promise.t) lazy_t

  val leaf_iterations : int

  val leaf_option : (field list * Stmt.t -> (trans * Proof.t) Promise.t) lazy_t

  val leaf_option_iterations : int

  val extend :
    (field list * (trans * Proof.t) -> (trans * Proof.t) Promise.t) lazy_t

  val extend_iterations : int

  val extend_option :
    (field list * (trans * Proof.t) -> (trans * Proof.t) Promise.t) lazy_t

  val extend_option_iterations : int

  type merge_input =
    { left : trans; left_proof : Proof.t; right : trans; right_proof : Proof.t }

  val merge : (merge_input -> (trans * Proof.t) Promise.t) lazy_t

  type tag_t

  type tag_var

  val tag : tag_var Compile_simple.tag lazy_t

  module Make : functor
    (Inputs : sig
       module Action_state : Rollup_state.Action_state_type

       val get_iterations : int
     end)
    -> sig
    type original_stmt_t := Stmt.t

    module Stmt : sig
      type t =
        { source : Inputs.Action_state.With_length.t
        ; target : Inputs.Action_state.With_length.t
        }
      [@@deriving snarky]
    end

    type t

    type var

    val typ : (var, t) Typ.t

    val get :
         ?check:Zeko_util.Boolean.var
      -> var
      -> (Stmt.var * tag_var Compile_simple.prev) Checked.t

    val make :
         proof_source:original_stmt_t
      -> proof_target:original_stmt_t
      -> ?proof:Proof.t
      -> original_stmt_t
      -> field list
      -> t
  end
end

module M_without_length : sig
  module Stmt = F
  module Init = Stmt

  val init : check:'a -> Init.var -> Stmt.var Checked.t

  val step : field_var -> Stmt.var -> Stmt.var Checked.t
end

module Without_length : sig
  module Stmt = M_without_length.Stmt

  type trans = { source : field; target : field }

  val leaf : (field list * field -> (trans * Proof.t) Promise.t) lazy_t

  val leaf_iterations : int

  val leaf_option : (field list * field -> (trans * Proof.t) Promise.t) lazy_t

  val leaf_option_iterations : int

  val extend :
    (field list * (trans * Proof.t) -> (trans * Proof.t) Promise.t) lazy_t

  val extend_iterations : int

  val extend_option :
    (field list * (trans * Proof.t) -> (trans * Proof.t) Promise.t) lazy_t

  val extend_option_iterations : int

  type merge_input =
    { left : trans; left_proof : Proof.t; right : trans; right_proof : Proof.t }

  val merge : (merge_input -> (trans * Proof.t) Promise.t) lazy_t

  type tag_t

  type tag_var

  val tag : tag_var Compile_simple.tag lazy_t

  module Make : functor
    (Inputs : sig
       module Action_state : Rollup_state.Action_state_type

       val get_iterations : int
     end)
    -> sig
    module Stmt : sig
      type t =
        { source : Inputs.Action_state.t; target : Inputs.Action_state.t }

      type var =
        { source : Inputs.Action_state.var; target : Inputs.Action_state.var }

      val typ : (var, t) Typ.t
    end

    type t

    type var

    val typ : (var, t) Typ.t

    val get :
         ?check:Zeko_util.Boolean.var
      -> var
      -> (Stmt.var * tag_var Compile_simple.prev) Checked.t

    val make :
         proof_source:field
      -> proof_target:field
      -> ?proof:Proof.t
      -> field
      -> field list
      -> t
  end
end
