[@@@warning "-67"]

open Snark_params.Tick
open Zeko_util

type tag_with_length_t

type tag_without_length_t

type tag_with_length_var

type tag_without_length_var

val tag_with_length :
  ( tag_with_length_var
  , tag_with_length_t
  , Pickles_types.Nat.N2.n
  , Pickles_types.Nat.N2.n )
  Pickles.Tag.t
  lazy_t

val tag_without_length :
  ( tag_without_length_var
  , tag_without_length_t
  , Pickles_types.Nat.N2.n
  , Pickles_types.Nat.N2.n )
  Pickles.Tag.t
  lazy_t

module Make_with_length (Inputs : sig
  module Action_state : Rollup_state.Action_state_type

  module Action : sig
    include SnarkType

    val to_actions_var : var -> Mina_base.Zkapp_account.Actions.var Checked.t
  end

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
    -> ( Stmt.var
       * ( tag_with_length_var
         , Pickles_types.Nat.N2.n )
         Pickles.Inductive_rule.Previous_proof_statement.t )
       Checked.t

  (** Prove the state machine execution. *)
  val prove : Action_state.With_length.t -> Action.t list -> t Async.Deferred.t
end

module Make_without_length (Inputs : sig
  module Action_state : Rollup_state.Action_state_type

  module Action : sig
    include SnarkType

    val to_actions_var : var -> Mina_base.Zkapp_account.Actions.var Checked.t
  end

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
    -> ( Stmt.var
       * ( tag_without_length_var
         , Pickles_types.Nat.N2.n )
         Pickles.Inductive_rule.Previous_proof_statement.t )
       Checked.t

  (** Prove the state machine execution. *)
  val prove : Action_state.t -> Action.t list -> t Async.Deferred.t
end
