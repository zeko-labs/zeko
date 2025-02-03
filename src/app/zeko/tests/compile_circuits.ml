let () =
  Promise.block_on_async_exn (fun () ->
      Zeko_circuits.Compile_simple.force_tag
        Zeko_circuits.Zeko_transaction_snark.tag )

(*
  let _out, _proof =
    let@ () = Promise.block_on_async_exn in
    base input
    *)
