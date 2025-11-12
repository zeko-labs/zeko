open Core_kernel
open Async
open Zeko_types
open Compile_simple
open Zeko_circuits

let compile_tag tag =
  Verification_key.of_tag tag |> Promise.to_deferred |> Deferred.ignore_m

let compile_all ~logger () =
  [%log info] "Compiling circuits" ;
  let start = Time.now () in
  let%bind () = compile_tag (Lazy.force Txn_rules.tag) in
  let%bind () = compile_tag (Lazy.force Ase.Without_length.tag) in
  let%bind () = compile_tag (Lazy.force Ase.With_length.tag) in
  let%bind () = compile_tag (Lazy.force Rule_commit.Verify_both_ases.tag) in
  let%bind () = compile_tag (Lazy.force Inner_rules_inst.tag) in
  let%bind () = compile_tag (Lazy.force Outer_rules_inst.tag) in
  let%bind () = compile_tag (Lazy.force Bridge_inst_mina.Check_accepted.tag) in
  let%bind () =
    compile_tag
      (Lazy.force
         Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_two_outer_ases
         .tag )
  in
  let%bind () =
    compile_tag
      (Lazy.force
         Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
         .Verify_check_accepted_and_ase
         .tag )
  in
  let%bind () =
    compile_tag (Lazy.force Bridge_inst_mina.System_L1_enabled.tag)
  in
  let%bind () =
    compile_tag (Lazy.force Bridge_inst_mina.System_L1_disabled.tag)
  in
  let%bind () = compile_tag (Lazy.force Bridge_inst_mina.System_L2.tag) in
  let%bind () =
    compile_tag (Lazy.force Bridge_inst_mina.System_L1_token_owner.tag)
  in
  [%log info] "Compiled circuits in %s"
    (Time.Span.to_string_hum (Time.diff (Time.now ()) start)) ;
  return ()
