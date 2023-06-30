module Js = Js_of_ocaml.Js

let rollup =
  object%js
    method vk : Mina_base.Side_loaded_verification_key.t =
      let module T = Transaction_snark.Make (struct
        let constraint_constants =
          Genesis_constants.Constraint_constants.compiled

        let proof_level = Genesis_constants.Proof_level.Full
      end) in
      let module M = Zkapps_rollup.Make (struct
        let tag = T.tag
      end) in
      Pickles.Side_loaded.Verification_key.of_compiled T.tag
  end
(*

      let module Proof = M.Proof in

      let step pk token_id txn =
        let account_update, () =
          Promise.block_on_async_exn
            (M.step
               { public_key = pk
               ; token_id
               ; may_use_token = Inherit_from_parent
               ; txn
               } )
        in
        account_update
      in
*)

let export () =
  Js.export "Snarky" Snarky_bindings.snarky ;
  Js.export "Ledger" Local_ledger.ledger_class ;
  Js.export "Pickles" Pickles_bindings.pickles ;
  Js.export "Test" Consistency_test.test ;
  Js.export "Rollup" rollup

let export_global () =
  let snarky_obj =
    Js.Unsafe.(
      let i = inject in
      obj
        [| ("Snarky", i snarky)
         ; ("Ledger", i Ledger.ledger_class)
         ; ("Pickles", i pickles)
         ; ("Test", i test)
         ; ("Rollup", i rollup)
        |])
  in
  Js.Unsafe.(set global (Js.string "__snarky") snarky_obj)
