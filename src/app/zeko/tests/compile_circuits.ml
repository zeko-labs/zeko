let () =
  Promise.block_on_async_exn (fun () ->
      Zeko_circuits.Compile_simple.force_tag
        Zeko_circuits.Zeko_transaction_snark.tag )

(*
let Zeko_circuits.Compile_simple.[ sync; _action ] =
  Zeko_circuits.Inner_rules.provers

(* FIXME *)
let public_key = Signature_lib.Public_key.Compressed.empty

let init_action_state = Mina_base.Zkapp_account.Actions.empty_state_element

let action1 : Zeko_circuits.Rollup_state.Inner_action.t =
  { aux = Snark_params.Tick.Field.zero
  ; children =
      Mina_base.Zkapp_command.Call_forest.With_hashes.of_account_updates []
  }

let action2 : Zeko_circuits.Rollup_state.Inner_action.t =
  { aux = Snark_params.Tick.Field.zero
  ; children =
      Mina_base.Zkapp_command.Call_forest.With_hashes.of_account_updates []
  }

let ase = Zeko_circuits.Ase.With_length.leaf_option

let witness : Zeko_circuits.Rule_inner_sync.Witness.t =
  { public_key; ase; vk_hash }

let () = Promise.block_on_async_exn @@ fun () -> sync witness

(*
  let _out, _proof =
    let@ () = Promise.block_on_async_exn in
    base input
    *)
    *)
