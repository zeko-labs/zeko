open Core_kernel
open Async
open Zeko_types
open Compile_simple
open Zeko_circuits

let compile_tag ~logger name tag =
  [%log info] "Compiling circuit: %s" name ;
  let%map () =
    Verification_key.of_tag tag |> Promise.to_deferred |> Deferred.ignore_m
  in
  [%log info] "Compiled circuit: %s" name

let compile_all ~logger () =
  [%log info] "Compiling circuits" ;
  let start = Time.now () in
  let%bind () =
    compile_tag ~logger "transaction rules" (Lazy.force Txn_rules.tag)
  in
  let%bind () =
    compile_tag ~logger "ASE without length" (Lazy.force Ase.Without_length.tag)
  in
  let%bind () =
    compile_tag ~logger "ASE with length" (Lazy.force Ase.With_length.tag)
  in
  let%bind () =
    compile_tag ~logger "commit ASE verification"
      (Lazy.force Rule_commit.Verify_both_ases.tag)
  in
  let%bind () =
    compile_tag ~logger "inner rules" (Lazy.force Inner_rules_inst.tag)
  in
  let%bind () =
    compile_tag ~logger "outer rules" (Lazy.force Outer_rules_inst.tag)
  in
  let%bind () =
    compile_tag ~logger "Mina bridge acceptance"
      (Lazy.force Bridge_inst_mina.Check_accepted.tag)
  in
  let%bind () =
    compile_tag ~logger "Ethereum asset bridge acceptance"
      (Lazy.force Bridge_inst_ethereum_token.Check_accepted.tag)
  in
  let%bind () =
    compile_tag ~logger "cancelled deposit outer ASE verification"
      (Lazy.force
         Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_two_outer_ases
         .tag )
  in
  let%bind () =
    compile_tag ~logger "cancelled deposit acceptance verification"
      (Lazy.force
         Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_check_accepted_and_ase
         .tag )
  in
  let%bind () =
    compile_tag ~logger "enabled Mina L1 bridge"
      (Lazy.force Bridge_inst_mina.System_L1_enabled.tag)
  in
  let%bind () =
    compile_tag ~logger "disabled Mina L1 bridge"
      (Lazy.force Bridge_inst_mina.System_L1_disabled.tag)
  in
  let%bind () =
    compile_tag ~logger "Mina L2 bridge"
      (Lazy.force Bridge_inst_mina.System_L2.tag)
  in
  let%bind () =
    compile_tag ~logger "universal Ethereum asset L2 bridge"
      (Lazy.force Bridge_inst_ethereum_token.System_L2.tag)
  in
  let%bind () =
    compile_tag ~logger "Ethereum asset registry"
      (Lazy.force Bridge_inst_ethereum_token.Registry.registry_tag)
  in
  let%bind () =
    compile_tag ~logger "Mina L1 token owner bridge"
      (Lazy.force Bridge_inst_mina.System_L1_token_owner.tag)
  in
  [%log info] "Compiled circuits in %s"
    (Time.Span.to_string_hum (Time.diff (Time.now ()) start)) ;
  return ()
