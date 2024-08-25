open Snark_params.Tick
open Zeko_util

module Make (Inputs : sig
  module Elem : SnarkType
  
  module Stmt : SnarkType
  
  module BaseWitness : SnarkType
  
  val base : BaseWitness.var -> Stmt.var
  
  val step : Elem.var -> Stmt.var -> Stmt.var
end) : sig
  open Inputs
  
  module Stmt = Stmt
  module Elem = Elem
  module BaseWitness = BaseWitness

  type t [@@deriving snarky]

  val get : ?check:Boolean.var -> var -> Stmt.var * (Stmt.var, Pickles_types.Nat.N2.n) Pickles.Inductive_rule.Previous_proof_statement.t

  val statement : t -> Stmt.t

  val tag :
    ( Stmt.var
    , Stmt.t
    , Pickles_types.Nat.N2.n
    , Pickles_types.Nat.N2.n )
    Pickles.Tag.t
    lazy_t

  val prove :
    source:F.t
    -> Mina_base.Zkapp_account.Actions.t list (** head newest, tail oldest *)
    -> t Async_kernel.Deferred.t
  
  val base :
    t Async_kernel.Deferred.t
  
  val step :
    t ->
    Elem.t list ->
    t Async_kernel.Deferred.t
end
