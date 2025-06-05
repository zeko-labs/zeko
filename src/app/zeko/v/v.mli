open Snark_params.Tick

type +'a t

val typ : ('a t, 'a) Typ.t

val get : 'a t -> 'a As_prover.t

val create : 'a As_prover.t -> 'a t Checked.t

val as_prover_value : 'a t -> 'a Typ.prover_value Checked.t

val unsafe_unwrap : 'a t -> 'a option

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val return : 'a -> 'a t
