open Core_kernel
open Zeko_util

let compilation_result =
  lazy
    (let@ () = Promise.block_on_async_exn in
     compile_simple ()
       ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
       ~name:"Inner_rules" ~override_wrap_domain:N0
       ~branches:[ Rule_inner_sync.rule ] )

let tag : (_, _, _, Pickles_types.Nat.N1.n) Pickles.Tag.t lazy_t =
  lazy
    ( match force compilation_result with
    | Compile_simple.Result { tag; provers = _; tag_length = S Z } ->
        tag )

let inner_sync input =  match force compilation_result with
  | Compile_simple.Result
      { tag = _; provers = [ prove_inner_sync ]; tag_length = _ } ->
      prove_inner_sync input

