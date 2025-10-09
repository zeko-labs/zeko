open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util

let height = 35

include Indexed_merkle_tree.Make (struct
  module Key = struct
    type t = Token_id.t

    type var = Token_id.Checked.t

    let typ = Token_id.typ
  end

  let assert_x_less_than_y_less_than_z ~(x : Key.var) ~(y : Key.var)
      ~(z : Key.var) =
    (* pretty sure of_field is supposed to be of_field_unsafe, and to_field_unsafe is supposed to be to_field *)
    let x = Token_id.Checked.to_field_unsafe x in
    let y = Token_id.Checked.to_field_unsafe y in
    let z = Token_id.Checked.to_field_unsafe z in
    let* () =
      with_label __LOC__
      @@ fun () -> Comparison_gadget.assert_greater_than_full z y
    in
    let*| () =
      with_label __LOC__
      @@ fun () -> Comparison_gadget.assert_greater_than_full y x
    in
    ()

  let height = height
end)
