open Core_kernel
open Zeko_util

let compilation_result =
  lazy
    (time "Inner_rules" (fun () ->
         Pickles.compile () ~cache:Cache_dir.cache
           ~public_input:(Output Mina_base.Zkapp_statement.typ)
           ~auxiliary_typ:V.typ
           ~branches:(module Pickles_types.Nat.N1)
           ~max_proofs_verified:(module Pickles_types.Nat.N1)
           ~name:"Inner_rules" ~override_wrap_domain:N0
           ~constraint_constants:
             (Genesis_constants.Constraint_constants.to_snark_keys_header
                constraint_constants )
           ~choices:(fun ~self:_ -> [ Rule_inner_sync.rule () ]) ) )

let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)
