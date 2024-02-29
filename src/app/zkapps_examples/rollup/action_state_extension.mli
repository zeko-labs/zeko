open Util
open Snark_params.Tick

module Stmt : sig
  type t = { source : F.t; target : F.t } [@@deriving show, snarky]
end

type t [@@deriving snarky]

val statement_var : var -> Stmt.var

val statement : t -> Stmt.t

val verify :
     ?check:Boolean.var
  -> var
  -> ( Stmt.var
     , Pickles_types.Nat.N2.n )
     Pickles.Inductive_rule.Previous_proof_statement.t

val tag :
  ( Stmt.var
  , Stmt.t
  , Pickles_types.Nat.N2.n
  , Pickles_types.Nat.N2.n )
  Pickles.Tag.t
  lazy_t

val prove :
     ?dummy:bool
  -> source:F.t
  -> Mina_base.Zkapp_account.Actions.t list (** head oldest, tail newest *)
  -> t Async_kernel.Deferred.t

val merge : t -> t -> t Async_kernel.Deferred.t
