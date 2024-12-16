include
  ( val Compile_simple.compile ()
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~name:"Inner_rules"
          ~branches:[ Rule_inner_sync.rule; Rule_inner_action_witness.rule ] )

(* FIXME: remove for lazy compilation *)
let () = Promise.block_on_async_exn (fun () -> Compile_simple.force_tag tag)
