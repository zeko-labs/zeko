open Core_kernel
open Zeko_util

module Make (T : Transaction_snark.S) = struct
  module Inputs = struct
    let macroslot_size = 256
  end

  module Macroslot_inst = Macroslot.Make (Inputs)
  module Outer_inst = Outer.Make (Inputs) (Macroslot_inst)
  module Rule_commit_inst =
    Rule_commit.Make (Inputs) (Macroslot_inst) (Outer_inst) (T)

  let compilation_result =
    lazy
      (time "Outer_rules" (fun () ->
           Pickles.compile () ~cache:Cache_dir.cache
             ~public_input:(Output Mina_base.Zkapp_statement.typ)
             ~auxiliary_typ:V.typ
             ~branches:(module Pickles_types.Nat.N1)
             ~max_proofs_verified:(module Pickles_types.Nat.N2)
             ~name:"Outer_rules" ~override_wrap_domain:N0
             ~constraint_constants:
               (Genesis_constants.Constraint_constants.to_snark_keys_header
                  constraint_constants )
             ~choices:(fun ~self:_ -> [ Rule_commit_inst.rule ]) ) )

  let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)
end
