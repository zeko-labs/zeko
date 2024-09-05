open Snark_params.Tick
open Zeko_util

[@@@warning "-67"]

module Make (Inputs : sig
  val macroslot_size : int
end) : sig
  include SnarkType
  
  val lower_var : var -> Slot.var Checked.t
  val upper_var : var -> Slot.var Checked.t
  val zero : t
  val to_field_var : var -> Snark_params.Tick.Field.Var.t
  val dec : var -> var Checked.t
end
