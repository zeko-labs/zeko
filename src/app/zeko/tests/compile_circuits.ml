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
  Async_unix.Thread_safe.block_on_async_exn (fun () ->
      Ase_inst.prove dummy [ action ] )

let () = printf "proved ase!\n"

module Example = struct
  module Witness = struct
    type t = { x : Ase_inst.t } [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main _ =
    let* { x } = exists_witness in
    let* _x, verify_x = Ase_inst.get x in
    Checked.return
      Pickles.Inductive_rule.
        { previous_proof_statements = [ verify_x ]
        ; public_output = ()
        ; auxiliary_output = ()
        }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "test"
    ; prevs = [ force Zkapps_rollup.Ase.tag_without_length ]
    ; main = (fun x -> main x |> Run.run_checked)
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }

  let () = printf "about to compile my circuit\n"

  let _tag, _cache, _proof_module, Pickles.Provers.[ prove ] =
    time "example" (fun () ->
        let ((tag, _, _, _) as r) =
          Pickles.compile () ~cache:Cache_dir.cache
            ~public_input:(Output Typ.unit) ~auxiliary_typ:Typ.unit
            ~branches:(module Pickles_types.Nat.N1)
            ~max_proofs_verified:(module Pickles_types.Nat.N1)
            ~name:"my example jduaihx" ~override_wrap_domain:N1
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self:_ -> [ rule ])
        in

        let (_ : Pickles.Side_loaded.Verification_key.t) =
          Async.Thread_safe.block_on_async_exn (fun () ->
              Pickles.Side_loaded.Verification_key.of_compiled tag )
        in
        r )

  let (), (), (_ : _ Pickles.Proof.t) =
    printf "proving jdauiwbxui\n" ;
    let dummy =
      Zkapps_rollup.Rollup_state.Inner_action_state.unsafe_value_of_field
        (Field.of_int 189247)
    in
    let action : Zkapps_rollup.Rollup_state.Inner.Action.t =
      { aux = Field.zero; children = [] }
    in
    let x =
      Async_unix.Thread_safe.block_on_async_exn (fun () ->
          Ase_inst.prove dummy [ action ] )
    in
    printf "proved action state extension\n" ;
    let r =
      let@ () = time "proving example circuit max_proofs_verified N1" in
      let@ () = Async.Thread_safe.block_on_async_exn in
      prove ~handler:(handler { x }) ()
    in

    r
end
