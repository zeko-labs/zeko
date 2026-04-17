module Make (Inputs : sig
  val chain_l2 : Mina_signature_kind.t

  val multisig_key : Multisig.t
end)
() =
struct
  module Rule_inner_sync_inst = Rule_inner_sync.Make (Inputs)
  module Rule_inner_action_witness_inst = Rule_inner_action_witness.Make (Inputs)

  module Rule_multisig_update_inst = Rule_multisig_update.Make (struct
    let chain = Inputs.chain_l2

    let multisig_key = Inputs.multisig_key
  end)

  include
    ( val Compile_simple.compile ()
            ~out_typ:
              Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
            ~name:"Inner_rules"
            ~branches:
              [ Rule_inner_sync_inst.rule
              ; Rule_inner_action_witness_inst.rule
              ; Rule_multisig_update_inst.rule
              ] )
end
