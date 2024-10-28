open Core_kernel
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

  let tag : (_, _, _, Pickles_types.Nat.N1.n) Pickles.Tag.t lazy_t =
    lazy
      ( match force compilation_result with
      | Compile_simple.Result { tag; provers = _; tag_length = S Z } ->
          tag )

  let commit input =  match force compilation_result with
    | Compile_simple.Result
        { tag = _; provers = [ commit ]; tag_length = _ } ->
        commit input

end
