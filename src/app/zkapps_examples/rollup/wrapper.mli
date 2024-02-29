open Util
open Snark_params.Tick

module Stmt : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    }
  [@@deriving snarky, yojson]
end

type t [@@deriving snarky]

val statement_var : var -> Stmt.var

val statement : t -> Stmt.t

val dummy_pc_init : Mina_base.Pending_coinbase.Stack.t

val dummy_pc : Mina_base.Pending_coinbase.Stack.t

val dummy_state_body : Mina_state.Protocol_state.Body.Value.t

val verify :
     var
  -> ( Stmt.var
     , Pickles_types.Nat.N2.n )
     Pickles.Inductive_rule.Previous_proof_statement.t

module Make (T : Transaction_snark.S) : sig
  val tag :
    ( Stmt.var
    , Stmt.t
    , Pickles_types.Nat.N2.n
    , Pickles_types.Nat.N2.n )
    Pickles.Tag.t

  val wrap : Transaction_snark.t -> t Async_kernel.Deferred.t

  val merge : t -> t -> t Async_kernel.Deferred.t

  (* FIXME: remove, unneeded *)
  val verify : t -> unit Core_kernel.Or_error.t Async_kernel.Deferred.t
end
