open Snark_params.Tick

type 'a t = 'a Typ.prover_value

val get : 'a t -> 'a

val create : (unit -> 'a) -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val if_ : Boolean.var -> then_:'a t -> else_:'a t -> 'a t

val typ : unit -> ('a t, 'a) Typ.t
