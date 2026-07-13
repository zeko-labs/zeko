open Core

module Field = Snark_params.Tick.Field
module PC = Signature_lib.Public_key.Compressed

let field_to_hex field =
  Kimchi_backend.Pasta.Basic.Bigint256.to_hex_string
    (Kimchi_backend.Pasta.Basic.Fp.to_bigint field)
  |> String.lowercase
  |> String.chop_prefix_if_exists ~prefix:"0x"
  |> fun hex -> String.make (64 - String.length hex) '0' ^ hex

let amount value =
  Unsigned.UInt64.of_string value |> Currency.Amount.of_uint64

let aux ~amount:value ~recipient_x ~recipient_is_odd =
  let params : Zeko_circuits.Bridge_state.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
    ; amount = amount value
    ; recipient =
        ({ x = Field.of_string recipient_x; is_odd = recipient_is_odd } : PC.t)
    ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
    }
  in
  Utils.value_to_hash ~init:Zeko_constants.ethereum_deposit_salt
    Zeko_circuits.Bridge_state.Deposit_params_base.typ params
  |> field_to_hex

let () =
  let vectors =
    [ ( "1000000000"
      , "16909060"
      , false
      , "2e9d1b29cea8eaba8c1dfe6d8c78b21127ce44a8378b3c9d2ee9ba0ddbd7c849"
      )
    ; ( "2000000000"
      , "84281096"
      , false
      , "1a03b5b4a38e241ee071764a843e5b7bf29aa0e455d7ccd53a83f729885bfb18"
      )
    ; ( "3000000000"
      , "151653132"
      , true
      , "1adc48d4e3b4478369ec2d8ce4ca72c397c9e75f019b24c9d65c262ae9757fa9"
      )
    ]
  in
  List.iter vectors ~f:(fun (amount, recipient_x, recipient_is_odd, expected) ->
      let actual = aux ~amount ~recipient_x ~recipient_is_odd in
      if not (String.equal actual expected) then
        failwithf "Ethereum deposit aux mismatch: expected %s, got %s" expected
          actual () )
