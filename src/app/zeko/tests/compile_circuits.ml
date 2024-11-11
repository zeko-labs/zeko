open Core_kernel
open Snark_params.Tick
open Zkapps_rollup.Zeko_util

(*
module T = Transaction_snark.Make (struct
  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  let proof_level : Genesis_constants.Proof_level.t = Full
end)

module M = Zkapps_rollup.Outer_rules.Make (T)
*)

module Ase_inst = Zkapps_rollup.Ase.Make_without_length (struct
  module Action_state = Zkapps_rollup.Rollup_state.Inner_action_state
  module Action = Zkapps_rollup.Rollup_state.Inner.Action

  let get_iterations = 0
end)

let (_ : _ Pickles.Tag.t) = force Zkapps_rollup.Ase.tag_with_length

let (_ : _ Pickles.Tag.t) = force Zkapps_rollup.Ase.tag_without_length

let _ase : Ase_inst.t =
  let dummy =
    Zkapps_rollup.Rollup_state.Inner_action_state.unsafe_value_of_field
      (Field.of_int 189247)
  in
  let action : Zkapps_rollup.Rollup_state.Inner.Action.t =
    { aux = Field.zero; children = [] }
  in
  Promise.block_on_async_exn (fun () -> Ase_inst.prove dummy [ action ])

let () = printf "proved ase!\n"

module Example = struct
  module Witness = struct
    type t = { x : Ase_inst.t } [@@deriving snarky]
  end

  let%snarkydef_ main (w : Witness.t V.t) =
    let* { x } = exists ~compute:(V.get w) Witness.typ in
    let* _x, verify_x = Ase_inst.get x in
    Checked.return
      Compile_simple.
        { out = ()
        ; prevs =
            ( match verify_x with
            | { proof_must_verify; proof; public_input } ->
                One_prev { proof_must_verify; proof; public_input } )
        }

  let rule : _ Compile_simple.branch =
    { branch_name = "test"
    ; tags = One_tag (Tag (force Zkapps_rollup.Ase.tag_without_length))
    ; main
    }

  let () =
    printf "about to compile my circuit\n" ;

    let (Compile_simple.Result { tag = _; provers = [ prove ]; tag_length = _ })
        =
      let@ () = Promise.block_on_async_exn in
      compile_simple ~out_typ:Typ.unit ~branches:[ rule ]
        ~name:"my example jduaihx" ~override_wrap_domain:N1 ()
    in

    printf "proving jdauiwbxui\n" ;
    let dummy =
      Zkapps_rollup.Rollup_state.Inner_action_state.unsafe_value_of_field
        (Field.of_int 189247)
    in
    let action : Zkapps_rollup.Rollup_state.Inner.Action.t =
      { aux = Field.zero; children = [] }
    in
    let x =
      Promise.block_on_async_exn (fun () -> Ase_inst.prove dummy [ action ])
    in
    printf "proved action state extension\n" ;
    let (), _proof =
      let@ () = time "proving example circuit max_proofs_verified N1" in
      let@ () = Promise.block_on_async_exn in
      prove { x }
    in
    ()
end
