open Core_kernel
open Zeko_util

let compilation_result =
  lazy
    (let@ () = Promise.block_on_async_exn in
     Compile_simple.compile ()
       ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
       ~name:"Inner_rules" ~override_wrap_domain:N0
       ~branches:[ Rule_inner_sync.rule; Rule_inner_action_witness.rule ] )

let tag : (_, _, _, Pickles_types.Nat.N2.n) Pickles.Tag.t lazy_t =
  lazy
    ( match force compilation_result with
    | Compile_simple.Result { tag; provers = _; tag_length = S (S Z) } ->
        tag )

let inner_sync input =
  match force compilation_result with
  | Compile_simple.Result
      { tag = _; provers = [ inner_sync; _ ]; tag_length = _ } ->
      inner_sync input

let action_witness input =
  match force compilation_result with
  | Compile_simple.Result
      { tag = _; provers = [ _; action_witness ]; tag_length = _ } ->
      action_witness input
