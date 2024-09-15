[@@@warning "-67"]

open Snark_params.Tick
open Zeko_util

(** Define a provable state machine as a machine with some initial state
    and some step function. *)
module Make : functor
  (Inputs : sig
     (** This is what the step function takes *)
     module Elem : SnarkType

     (** This is what the step function takes when a step might not happen. *)
     module ElemOption : SnarkType

     (** Morally equivalent to `Some`. *)
     val elem_to_option : Elem.t -> ElemOption.t

     (** Morally equivalent to `None`, but you may use some dummy value. *)
     val elem_option_none : ElemOption.t

     (** This is the statement you're trying to prove. *)
     module Stmt : SnarkType

     (** This is passed to `init` when initializing the state machine. This way, you can have several starting points. *)
     module Init : SnarkType

     val init :
          check:Zeko_util.Boolean.var option
            (** The circuit must not fail if this is false. *)
       -> Init.var
       -> Stmt.var Checked.t

     val step :
          check:Zeko_util.Boolean.var option
            (** The circuit must not fail if this is false. *)
       -> Elem.var
       -> Stmt.var
       -> Stmt.var Checked.t

     val step_option :
          check:Zeko_util.Boolean.var option
            (** The circuit must not fail if this is false. *)
       -> ElemOption.var
       -> Stmt.var
       -> Stmt.var Checked.t

     (** Set this as high as possible. Makes recursive circuits bigger. *)
     val leaf_iterations : int

     (** Set this as high as possible. Makes recursive circuits bigger. *)
     val leaf_option_iterations : int

     (** Set this as high as possible. Makes recursive circuits bigger. *)
     val extend_iterations : int

     (** Set this as high as possible. Makes recursive circuits bigger. *)
     val extend_option_iterations : int

     (** Name of state machine for debugging purposes. *)
     val name : string

     (** The size of the circuit. Set to None to deduce automatically via default Pickles mechanism. *)
     val override_wrap_domain : Pickles_base.Proofs_verified.t option
   end)
  -> sig
  open Inputs

  module Trans : sig
    type t = { source : Stmt.t; target : Stmt.t } [@@deriving snarky]
  end

  (** The tag for the Pickles rule. You need to specify this in your rule. *)
  val tag :
    ( Trans.var
    , Trans.t
    , Pickles_types.Nat.N2.n
    , Pickles_types.Nat.N2.n )
    Pickles.Tag.t
    lazy_t

  module Make : functor
    (Inputs : sig
       (** Set this as high as possible. Makes your circuit bigger. *)
       val get_iterations : int
     end)
    -> sig
    include SnarkType

    (** You should pass this directly into previous_proof_statements *)
    val get :
         ?check:Zeko_util.Boolean.var
           (** Set this to false if you don't want to check the proof after all. *)
      -> var (** What you're trying to verify *)
      -> ( Trans.var
         * ( Trans.var
           , Pickles_types.Nat.N2.n )
           Pickles.Inductive_rule.Previous_proof_statement.t )
         Checked.t

    (** Prove the state machine execution. *)
    val prove : Init.t -> Elem.t list -> t Async.Deferred.t
  end
end
