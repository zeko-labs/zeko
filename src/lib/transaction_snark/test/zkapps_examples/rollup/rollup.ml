open Transaction_snark_tests.Util
open Core_kernel
open Mina_base
open Signature_lib
module Impl = Pickles.Impls.Step
module Inner_curve = Snark_params.Tick.Inner_curve
module Nat = Pickles_types.Nat
module Local_state = Mina_state.Local_state
module Zkapp_command_segment = Transaction_snark.Zkapp_command_segment
module Statement = Transaction_snark.Statement

let gen_keys () =
  let kp = Keypair.create () in
  (Public_key.compress kp.public_key, kp.private_key)

let fee_to_create n =
  Genesis_constants.Constraint_constants.compiled.account_creation_fee
  |> Currency.Amount.of_fee
  |> (fun x -> Currency.Amount.scale x n)
  |> Option.value_exn

let fee_to_create_signed n =
  fee_to_create n |> Currency.Amount.Signed.of_unsigned
  |> Currency.Amount.Signed.negate

let int_to_amount amt =
  let magnitude, sgn = if amt < 0 then (-amt, Sgn.Neg) else (amt, Sgn.Pos) in
  { Currency.Signed_poly.magnitude =
      Currency.Amount.of_nanomina_int_exn magnitude
  ; sgn
  }

let%test_module "Tokens test" =
  ( module struct
    let () = Base.Backtrace.elide := false

    let pk, sk = gen_keys ()

    let token_id = Token_id.default

    let account_id = Account_id.create pk token_id

    let owned_token_id = Account_id.derive_token_id ~owner:account_id

    module T = Transaction_snark.Make (struct
      let constraint_constants = Genesis_constants.Constraint_constants.compiled

      let proof_level = Genesis_constants.Proof_level.Full
    end)

    module M = Zkapps_rollup.Make (struct
      let tag = T.tag
    end)

    let mint_to_keys = gen_keys ()

    module Account_updates = struct
      let deploy ~balance_change =
        Zkapps_examples.Deploy_account_update.full ~balance_change
          ~access:Either pk token_id M.vk

      let init =
        let account_update, () =
          Async.Thread_safe.block_on_async_exn
            (M.init
               { public_key = pk
               ; token_id
               ; may_use_token = Inherit_from_parent
               } )
        in
        account_update

      let step txn_snark txn_snark_proof =
        let account_update, () =
          Async.Thread_safe.block_on_async_exn
            (M.step
               { public_key = pk
               ; token_id
               ; may_use_token = Inherit_from_parent
               ; txn_snark
               ; txn_snark_proof
               } )
        in
        account_update
    end

    let signers = [| (pk, sk); mint_to_keys |]

    let initialize_ledger ledger =
      let balance =
        let open Currency.Balance in
        let add_amount x y = add_amount y x in
        zero
        |> add_amount (Currency.Amount.of_nanomina_int_exn 500)
        |> Option.value_exn
        |> add_amount (fee_to_create 50)
        |> Option.value_exn
      in
      let account = Account.create account_id balance in
      let _, loc =
        Ledger.get_or_create_account ledger account_id account
        |> Or_error.ok_exn
      in
      loc

    let finalize_ledger loc ledger = Ledger.get ledger loc

    let%test_unit "Initialize and mint" =
      let account =
        []
        (* |> Zkapp_command.Call_forest.cons_tree Account_updates.mint *)
        (* |> Zkapp_command.Call_forest.cons_tree Account_updates.initialize *)
        |> Zkapp_command.Call_forest.cons
             (Account_updates.deploy ~balance_change:(fee_to_create_signed 1))
        |> test_zkapp_command ~fee_payer_pk:pk ~signers ~initialize_ledger
             ~finalize_ledger
      in
      ignore account
  end )
