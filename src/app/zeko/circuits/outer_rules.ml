open Core_kernel

module Inputs = struct
  let max_valid_while_size = 128

  let inner_public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x
        @@ Snark_params.Tick.Field.of_int 123456789)
    in
    Signature_lib.Public_key.compress pk
end

module Rule_commit_inst = Rule_commit.Make (Inputs)

include
  ( val Compile_simple.compile ()
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:
            [ Rule_commit_inst.rule; Rule_action_witness.rule; Rule_pause.rule ]
          ~name:"Outer_rules" )
