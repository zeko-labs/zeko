open Snark_params.Tick

type 'a t

val typ : ('a t, 'a) Typ.t

val get : 'a t -> 'a As_prover.t

val create : 'a As_prover.t -> 'a t Checked.t

val as_ref : 'a t -> 'a As_prover.Ref.t

val unsafe_unwrap : 'a t -> 'a option

val map : f:('a -> 'b) -> 'a t -> 'b t

val return : 'a -> 'a t
