open Core_kernel
open Async
open Sequencer_lib
open Signature_lib
open Mina_base
module Field = Snark_params.Tick.Field

let printf = Core.printf

let run = Thread_safe.block_on_async_exn

let archive_uri : Uri.t Cli_lib.Flag.Types.with_name =
  Cli_lib.Flag.Types.
    { value = Uri.of_string "https://api.minascan.io/archive/devnet/v1/graphql"
    ; name = "archive-uri"
    }

let () =
  run
  @@ fun () ->
  let%map actions =
    Gql_client.fetch_actions archive_uri
      (Public_key.Compressed.of_base58_check_exn
         "B62qqmWLGXhbmnUx26mvQDtWL5fQRJWwArxDA8ARErfW14bYYGWvXA9" )
  in
  printf
    !"empty_hash: %{sexp: Field.t}\n%!"
    Zkapp_account.Actions_impl.empty_hash ;
  List.iter actions
    ~f:(fun (fields, _block_height, `Before before, `After after) ->
      printf
        !"fields: %{sexp: Field.t}\n%!"
        (Zkapp_account.Actions_impl.hash fields) ;
      printf !"b: %{sexp: Field.t}\n%!" before ;
      printf !"a: %{sexp: Field.t}\n%!" after )

let () =
  printf
    !"empty forest: %{sexp: Zkapp_command.Digest.Forest.t}\n%!"
    (Zkapp_command.Call_forest.hash [])

let () =
  let random_pk = (Keypair.create ()).public_key |> Public_key.compress in
  let deposit_params : Zeko_types.Bridge.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = random_pk
    ; amount = Currency.Amount.one
    ; recipient = random_pk
    ; timeout = Zeko_circuits.Zeko_util.Slot.max_value
    }
  in
  printf "random pk: %s\n%!" (Public_key.Compressed.to_base58_check random_pk) ;
  printf
    !"deposit params hash: %{sexp: Field.t}\n%!"
    (Utils.value_to_hash ~init:Zeko_constants.deposit_salt
       Zeko_circuits.Bridge_state.Deposit_params_base.typ deposit_params )
