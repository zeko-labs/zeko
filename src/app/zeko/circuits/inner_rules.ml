include
  ( val Compile_simple.compile ()
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~name:"Inner_rules" ~override_wrap_domain:`N1
          ~branches:[ Rule_inner_sync.rule; Rule_inner_action_witness.rule ] )
