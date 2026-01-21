module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end)
() =
struct
  module Rule_emergency_da_inst = Rule_emergency_da.Make (Inputs)

  include
    ( val Compile_simple.compile ()
            ~out_typ:
              Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
            ~branches:[ Rule_emergency_da_inst.rule ]
            ~name:"Emergency_da_rules" )
end
