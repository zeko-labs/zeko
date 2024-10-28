open Zeko_util

module Make (T : Transaction_snark.S) = struct
  module Inputs = struct
    let max_valid_while_size = 128
  end

  module Rule_commit_inst = Rule_commit.Make (Inputs) (T)

  let compilation_result =
    lazy
      (let@ () = Promise.block_on_async_exn in
       compile_simple ()
         ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
         ~branches:[ Rule_commit_inst.rule ] (* add Rule_action_witness back *)
         ~name:"Outer_rules" )
end
