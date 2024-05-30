[@@@warning "-8-4"] (* ignore partial match and fragile-match warning *)

open Core
open Mina_ledger
open Currency
open Signature_lib
module U = Transaction_snark_tests.Util
open Mina_base
module For_tests = Mina_transaction_logic.For_tests

type call_forest = Zeko_util.call_forest

type call_forest_tree = Zeko_util.call_forest_tree

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let depth = constraint_constants.ledger_depth

let state_view = Mina_state.Protocol_state.Body.view U.genesis_state_body

let global_slot = state_view.global_slot_since_genesis

let pretty_print_cmd (cmd : Zkapp_command.t) : unit =
  let rec strip_tree (tree : call_forest_tree) : call_forest_tree =
    { tree with
      account_update = { tree.account_update with authorization = None_given }
    ; calls = strip_forest tree.calls
    }
  and strip_forest (forest : call_forest) : call_forest =
    List.map ~f:(fun tree -> { tree with elt = strip_tree tree.elt }) forest
  in

  let cmd =
    { cmd with
      fee_payer = { cmd.fee_payer with authorization = Signature.dummy }
    ; account_updates = strip_forest cmd.account_updates
    }
  in
  printf "%s\n" @@ Sexp.to_string_hum (Zkapp_command.sexp_of_t cmd)

let check_no_failure
    (applied : Ledger.Transaction_applied.Zkapp_command_applied.t) cmd =
  match applied.command.status with
  | Applied ->
      ()
  | Failed failuress ->
      List.iteri
        ~f:(fun i failures ->
          printf "Failures of account update %i:\n" i ;
          List.iter failures
            ~f:
              Transaction_status.Failure.(
                fun failure ->
                  printf "%s: %s\n" (to_string failure) (describe failure)) )
        failuress ;
      pretty_print_cmd cmd ;
      failwith "check_no_failure failed"

let ledger_apply ~global_slot ~state_view ledger cmd : unit =
  match
    Ledger.apply_zkapp_command_unchecked ~constraint_constants ~global_slot
      ~state_view ledger cmd
  with
  | Ok (applied, _) ->
      check_no_failure applied cmd
  | Error e ->
      pretty_print_cmd cmd ; Error.raise e

type tree = Tree of Account_update.t * tree list

let rec flatten_tree (depth : int) (Tree (root, children)) =
  (depth, root) :: List.concat_map ~f:(flatten_tree (depth + 1)) children

let mk_cmd (fee_payer : Account_update.Fee_payer.t) (acups : tree list) :
    Zkapp_command.t =
  let acups = List.concat_map ~f:(flatten_tree 0) acups in
  { fee_payer
  ; account_updates =
      Zkapp_command.Call_forest.(
        accumulate_hashes'
        @@ of_account_updates_map
             ~f:(fun (_, acup) -> acup)
             ~account_update_depth:(fun (depth, _) -> depth)
             acups)
  ; memo = Signed_command_memo.empty
  }

let amount_of_int : int -> Amount.Signed.t =
 fun n ->
  let r = Amount.of_nanomina_int_exn (abs n) |> Amount.Signed.of_unsigned in
  if n < 0 then Amount.Signed.negate r else r

let main () =
  let fee_payer = Signature_lib.Keypair.create () in
  let fee_nonce = ref 0 in
  let pay_fee : int -> unit -> Account_update.Fee_payer.t =
   fun n_creations () ->
    let r : Account_update.Fee_payer.t =
      { body =
          { public_key = Public_key.compress fee_payer.public_key
          ; fee =
              Fee.(
                to_nanomina_int constraint_constants.account_creation_fee
                * n_creations
                |> of_nanomina_int_exn)
          ; valid_until = None
          ; nonce = Account.Nonce.of_int !fee_nonce
          }
      ; authorization = Signature.dummy
      }
    in
    fee_nonce := 1 + !fee_nonce ;
    r
  in
  let pay : Keypair.t -> int -> Account_update.t =
   fun kp amount ->
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress kp.public_key
        ; use_full_commitment = true
        ; balance_change = amount_of_int (-amount)
        ; authorization_kind = Signature
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let l = Ledger.create_ephemeral ~depth () in
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (fee_payer, Int64.max_value) |]
    l ;
  let owner_kp = Signature_lib.Keypair.create () in
  let owner : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress owner_kp.public_key
        ; implicit_account_creation_fee = false
        }
    ; authorization = None_given
    }
  in
  let owned_kp = Signature_lib.Keypair.create () in
  let owned : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress owned_kp.public_key
        ; token_id =
            Account_id.derive_token_id
              ~owner:(Account_id.of_public_key owner_kp.public_key)
        ; implicit_account_creation_fee = false
        ; may_use_token = Parents_own_token
        ; balance_change = amount_of_int 1_000_000_000
        }
    ; authorization = None_given
    }
  in
  let creation_fee =
    constraint_constants.account_creation_fee |> Fee.to_nanomina_int
  in
  let cmd =
    mk_cmd (pay_fee 0 ())
      [ Tree (pay fee_payer (2 * creation_fee), [])
      ; Tree (owner, [ Tree (owned, []) ])
      ]
  in
  ledger_apply ~global_slot ~state_view l cmd ;
  ()

let () = main ()
