open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util

module type ASSET = sig
  module Witness : SnarkType

  type verified

  val verify : Witness.var -> verified Checked.t

  val vault_public_key : verified -> PC.var

  val token_id_l2 : verified -> Token_id.Checked.t

  val call_data : verified -> Currency.Amount.var -> F.var Checked.t

  val authenticated_registry_call :
    verified -> Account_update.Checked.t option

  val is_custom_token : bool
end

module Make (Inputs : sig
  val chain_l2 : Mina_signature_kind.t

  module Asset : ASSET
end) =
struct
  open Inputs

  module Witness = struct
    type t =
      { public_key : PC.t
      ; vk_hash : F.t
      ; asset : Asset.Witness.t
      ; amount : Currency.Amount.t
      }
    [@@deriving snarky]
  end

  (** Allow receiving any amount, even though access = Proof. *)
  let main (w : Witness.t V.t) =
    with_label ("main " ^ __LOC__) (fun () ->
        let* Witness.{ public_key; vk_hash; asset; amount } =
          exists Witness.typ ~compute:(V.get w)
        in
        let* verified_asset = Asset.verify asset in
        let token_id_l2 = Asset.token_id_l2 verified_asset in
        let* () =
          assert_equal ~label:"bridge receive vault public key" PC.typ
            public_key (Asset.vault_public_key verified_asset)
        in
        let* call_data = Asset.call_data verified_asset amount in
        let account_update =
          { default_account_update with
            public_key
          ; token_id = token_id_l2
          ; use_full_commitment =
              constant Boolean.typ (not Asset.is_custom_token)
          ; implicit_account_creation_fee =
              constant Boolean.typ (not Asset.is_custom_token)
          ; may_use_token =
              constant Account_update.May_use_token.typ Parents_own_token
          ; authorization_kind = authorization_vk_hash vk_hash
          ; balance_change = Currency.Amount.Signed.Checked.(of_unsigned amount)
          ; call_data
          }
        in
        let calls : Calls.t =
          match Asset.authenticated_registry_call verified_asset with
          | Some call ->
            [ (call, []) ]
          | None ->
              []
        in
        let*| out =
          make_outputs ~chain:chain_l2 account_update calls
        in
        Compile_simple.{ prevs = No_prevs; out } )

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "inner receive"; tags = No_tags; main }
end
