[@@@warning "-67"]

open Snark_params.Tick
open Zeko_util

type tag_branches

(** Define a provable state machine as a machine with some initial state
    and some step function. *)
module Make (Inputs : sig
  (** This is what the step function takes *)
  module Elem : SnarkType

  (** Used to fill remainder of circuit with dummy values. *)
  val dummy_elem : Elem.t

  (** This is the statement you're trying to prove. *)
  module Stmt : SnarkType

  (** This is passed to `init` when initializing the state machine. This way, you can have several starting points. *)
  module Init : SnarkType

  (* TODO: Allow verifying proofs inside. *)
  val init :
       check:Zeko_util.Boolean.var option
         (** The circuit must not fail if this is false. *)
    -> Init.var
    -> Stmt.var Checked.t

  (** Step function. This must not fail when given dummy_elem. *)
  val step : Elem.var -> Stmt.var -> Stmt.var Checked.t

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
  val wrap_domain : [ `N13 | `N14 | `N15 ] option
end)
() : sig
  open Inputs

  type trans = { source : Stmt.t; target : Stmt.t }

  type t := trans * Proof.t

  val leaf : Elem.t list * Stmt.t -> (trans * unit * Proof.t) Promise.t

  val leaf_iterations : int

  val leaf_option : Elem.t list * Stmt.t -> (trans * unit * Proof.t) Promise.t

  val leaf_option_iterations : int

  val extend : Elem.t list * t -> (trans * unit * Proof.t) Promise.t

  val extend_iterations : int

  val extend_option : Elem.t list * t -> (trans * unit * Proof.t) Promise.t

  val extend_option_iterations : int

  type merge_input =
    { left : trans; left_proof : Proof.t; right : trans; right_proof : Proof.t }

  val merge : merge_input -> (trans * unit * Proof.t) Promise.t

  type tag_t

  type tag_var

  (** The tag for the Pickles rule. You need to specify this in your rule. *)
  val tag : tag_var Compile_simple.tag

  module Make : functor
    (Inputs : sig
       (** Set this as high as possible. Makes your circuit bigger. *)
       val get_iterations : int
     end)
    -> sig
    include SnarkType

    (** You should pass this directly into previous_proof_statements *)
    val get :
         ?check:Boolean.var
           (** Set this to false if you don't want to check the proof after all. *)
      -> var (** What you're trying to verify *)
      -> (Stmt.var * tag_var Compile_simple.prev) Checked.t

    val get_full :
         ?check:Boolean.var
           (** Set this to false if you don't want to check the proof after all. *)
      -> var (** What you're trying to verify *)
      -> ( [ `Source of Stmt.var ]
         * [ `Target of Stmt.var ]
         * tag_var Compile_simple.prev )
         Checked.t

    val make :
         proof_source:Stmt.t
      -> proof_target:Stmt.t
      -> ?proof:Proof.t
      -> Init.t
      -> Elem.t list
      -> t

    val get_iterations : int
  end
end
