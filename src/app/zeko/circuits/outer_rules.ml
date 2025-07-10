module Make (Inputs : sig
  val max_valid_while_size : int

  val inner_public_key : Signature_lib.Public_key.Compressed.t

  val chain_l1 : Mina_signature_kind.t
end)
() =
struct
  module Rule_commit_inst = Rule_commit.Make (Inputs)
  module Rule_action_witness_inst = Rule_action_witness.Make (Inputs)
  module Rule_pause_inst = Rule_pause.Make (Inputs)

  include
    ( val Compile_simple.compile ()
            ~out_typ:
              Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
            ~auxiliary_typ:Snark_params.Tick.Typ.unit
            ~branches:
              [ Rule_commit_inst.rule
              ; Rule_action_witness_inst.rule
              ; Rule_pause_inst.rule
              ]
            ~name:"Outer_rules" )
end
