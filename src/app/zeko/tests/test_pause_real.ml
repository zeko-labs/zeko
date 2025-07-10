open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

module Outer_rules_inst =
  Outer_rules.Make
    (struct
      let max_valid_while_size = 1024

      let inner_public_key =
        let pk =
          Snark_params.Tick.Inner_curve.(
            to_affine_exn @@ point_near_x
            @@ Snark_params.Tick.Field.of_int 123456789)
        in
        Signature_lib.Public_key.compress pk

      let chain_l1 = Mina_signature_kind.Testnet
    end)
    ()

let Compile_simple.[ _commit; _action; pause ] = Outer_rules_inst.provers

let point_of_string_even s : Zeko_util.Even_PC.t =
  let x, _ =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  in
  { public_key = x }

let point_of_string s =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  |> Public_key.compress

let pause_witness : Rule_pause.Witness.t =
  { public_key = point_of_string "1238881"
  ; vk_hash = Field.of_string "19944541415"
  ; pause_key = point_of_string_even "1511111121"
  }

let _stmt, (), _proof =
  Promise.block_on_async_exn @@ fun () -> pause pause_witness
