open Zeko_util

module Make (Inputs : sig
  val macroslot_size : int
end) = struct
  include Slot

  let one = Mina_numbers.Global_slot_span.(Snark_params.Tick.constant typ @@ of_int 1)
  
  let dec (macroslot : var) =
    Slot.Checked.sub macroslot one |> run
  
  let inc (macroslot : var) =
    Slot.Checked.add macroslot one |> run
  
  let lower_var (macroslot : var) =
    Slot.Checked.(
      mul macroslot (constant @@ Slot.of_int Inputs.macroslot_size))
    |> run

  (* valid_while precondition is an inclusive range *)
  let upper_var (macroslot : var) =
    Slot.Checked.sub (lower_var (inc macroslot)) one |> run

  let to_field_var = Slot.Checked.to_field
end

