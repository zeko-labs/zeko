module Make (Inputs : sig
  val chain_l2 : Mina_signature_kind.t
end)
() =
struct
  module Rule_inner_sync_inst = Rule_inner_sync.Make (Inputs)
  module Rule_inner_action_witness_inst = Rule_inner_action_witness.Make (Inputs)

  include
    ( val Compile_simple.compile ()
            ~out_typ:
              Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
            ~auxiliary_typ:Snark_params.Tick.Typ.unit ~name:"Inner_rules"
            ~branches:
              [ Rule_inner_sync_inst.rule; Rule_inner_action_witness_inst.rule ]
      )
end
