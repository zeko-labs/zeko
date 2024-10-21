open Core_kernel
open Zeko_util

module Make (T : Transaction_snark.S) = struct
  module Inputs = struct
    let max_valid_while_size = 128
  end

  module Rule_commit_inst = Rule_commit.Make (Inputs) (T)

  let compilation_result =
    lazy
      (time "Outer_rules" (fun () ->
           Pickles.compile () ~cache:Cache_dir.cache
             ~public_input:(Output Mina_base.Zkapp_statement.typ)
             ~auxiliary_typ:V.typ
             ~branches:(module Pickles_types.Nat.N2)
             ~max_proofs_verified:(module Pickles_types.Nat.N3)
             ~name:"Outer_rules" ~override_wrap_domain:N0
             ~constraint_constants:
               (Genesis_constants.Constraint_constants.to_snark_keys_header
                  constraint_constants )
             ~choices:(fun ~self:_ ->
               [ Rule_commit_inst.rule; Rule_action_witness.rule ] ) ) )

  let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)
end
