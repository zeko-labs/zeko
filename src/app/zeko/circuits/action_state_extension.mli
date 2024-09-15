[@@@warning "-67"]

open Zeko_util
open Snark_params.Tick

module Action_state : sig
  type t = { action_state : F.t } [@@deriving snarky]
end

module Stmt : sig
  type t = { source : Action_state.t; target : Action_state.t }
  [@@deriving snarky]
end

val tag :
  ( Stmt.var
  , Stmt.t
  , Pickles_types.Nat.N2.n
  , Pickles_types.Nat.N2.n )
  Pickles.Tag.t
  lazy_t

module Make : functor
  (Inputs : sig
     val get_iterations : int
   end)
  -> sig
  include SnarkType

  val get :
       ?check:Boolean.var
    -> var
    -> ( Stmt.var
       * ( Stmt.var
         , Pickles_types.Nat.N2.n )
         Pickles.Inductive_rule.Previous_proof_statement.t )
       Checked.t

  val prove :
    Action_state.t -> Mina_base.Zkapp_account.Actions.t list -> t Async.Deferred.t
end
