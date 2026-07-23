open Core_kernel
open Zeko_types

let () =
  let (_ : Signature_lib.Public_key.Compressed.t) =
    Option.value_exn Zeko_circuits_config.Inputs.ethereum_holder_account_l1
      ~message:"ZEKO_ETHEREUM_BRIDGE_ADDRESS must be set"
  in
  let tag = Lazy.force Bridge_inst_ethereum.System_L2.tag in
  let verification_key =
    Promise.block_on_async_exn (fun () ->
        Compile_simple.Verification_key.of_tag tag )
  in
  verification_key |> Compile_simple.Verification_key.hash
  |> Snark_params.Tick.Field.to_string
  |> printf "Ethereum immediate-deposit bridge VK: %s\n"
