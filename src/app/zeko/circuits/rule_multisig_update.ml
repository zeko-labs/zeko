open Zeko_util
open Snark_params.Tick
open Mina_base

open struct
  module A = struct
    include Account_update.Body

    type var = Checked.t

    let typ = typ ()
  end
end

module Witness = struct
  type t = { multisig : Multisig.Witness.t; a : A.t } [@@deriving snarky]
end

module Make (Inputs : sig
  val chain : Mina_signature_kind.t

  val multisig_key : Multisig.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ multisig; a } = exists ~compute:(V.get w) Witness.typ in
    let* payload =
      make_checked
      @@ fun () ->
      Zkapp_command.Call_forest.Digest.Account_update.Checked.create
        ~signature_kind:chain a
    in
    let payload = (payload :> Field.Var.t) in
    let* () = Multisig.check ~signature_kind:chain multisig payload in
    let* multisig_key' = Multisig.of_witness_var multisig in
    let multisig_key = Field.Var.constant (Multisig.commit multisig_key) in
    let* () =
      assert_equal ~label:__LOC__ Field.typ multisig_key multisig_key'
    in
    let*| out = make_outputs ~chain a [] in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "zeko multisig update"; tags = No_tags; main }
end
