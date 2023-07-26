module Js = Js_of_ocaml.Js

let rollup_commitments =
  object%js
    val paymentCommitmentFromBase64 =
      fun (base64_payment : Js.js_string Js.t) ->
        match
          Mina_base.Signed_command.of_base64 @@ Js.to_string @@ base64_payment
        with
        | Error _ ->
            Util.raise_error "Invalid base64 payment"
        | Ok tx ->
            let roi = Mina_base.Signed_command.to_input_legacy tx.payload in
            Random_oracle.Legacy.pack_input roi |> Js.array

    val zkappCommandCommitmentFromBase64 =
      fun (base64_zkapp_command : Js.js_string Js.t) ->
        match
          Mina_base.Zkapp_command.of_base64 @@ Js.to_string
          @@ base64_zkapp_command
        with
        | Error _ ->
            Util.raise_error "Invalid base64 zkapp command"
        | Ok zkapp_command ->
            let commitment = Mina_base.Zkapp_command.commitment zkapp_command in
            let fee_payer_hash =
              Mina_base.Zkapp_command.Digest.Account_update.create
              @@ Mina_base.Account_update.of_fee_payer zkapp_command.fee_payer
            in
            Mina_base.Zkapp_command.Transaction_commitment.create_complete
              commitment
              ~memo_hash:(Mina_base.Signed_command_memo.hash zkapp_command.memo)
              ~fee_payer_hash
  end

let export () =
  Js.export "Snarky" Snarky_bindings.snarky ;
  Js.export "Ledger" Local_ledger.ledger_class ;
  Js.export "RollupCommitments" rollup_commitments ;
  Js.export "Pickles" Pickles_bindings.pickles ;
  Js.export "Test" Consistency_test.test

let export_global () =
  let snarky_obj =
    Js.Unsafe.(
      let i = inject in
      obj
        [| ("Snarky", i Snarky_bindings.snarky)
         ; ("Ledger", i Local_ledger.ledger_class)
         ; ("Pickles", i Pickles_bindings.pickles)
         ; ("Test", i Consistency_test.test)
        |])
  in
  Js.Unsafe.(set global (Js.string "__snarky") snarky_obj)
