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

let ledger_apply ~env ~global_slot ~state_view ledger cmd : unit =
  match
    Ledger.apply_zkapp_command_unchecked ~env ~constraint_constants ~global_slot
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

let get_account' ledger (account_id : Account_id.t) =
  let (`Existed : [ `Added | `Existed ]), loc =
    Or_error.ok_exn
    @@ Ledger.get_or_create_account ledger account_id
         (Account.initialize account_id)
  in
  let account = Option.value_exn @@ Ledger.get ledger loc in
  account

let get_account ledger (kp : Keypair.t) =
  get_account' ledger (Account_id.of_public_key kp.public_key)

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
  let l = Ledger.create_ephemeral ~depth () in
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (fee_payer, Int64.max_value) |]
    l ;
  let actions =
    (Option.value_exn (get_account l fee_payer).zkapp).action_state
  in
  let p = Format.pp_print_list Format.pp_print_string in
  Format.printf "%a\n\n" p
    ( Pickles_types.Vector.to_list actions
    |> List.map ~f:Pasta_bindings.Fp.to_string ) ;
  assert (
    Pickles_types.Vector.Vector_5.equal Pasta_bindings.Fp.equal actions
      [ Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ] ) ;
  let submit_action : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress fee_payer.public_key
        ; actions = [ [| Pickles.Backend.Tick.Field.zero |] ]
        ; authorization_kind = Signature
        ; use_full_commitment = true
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let set : Account_id.Set.t ref = ref Account_id.Set.empty in
  let env =
    object
      method mark_shifted_and_get_previous_shiftedness a =
        if Account_id.Set.mem !set a then false
        else (
          set := Account_id.Set.add !set a ;
          true )
    end
  in
  mk_cmd (pay_fee 0 ()) [ Tree (submit_action, []) ]
  |> ledger_apply ~env ~global_slot ~state_view l ;
  let actions =
    (Option.value_exn (get_account l fee_payer).zkapp).action_state
  in
  Format.printf "%a\n\n" p
    ( Pickles_types.Vector.to_list actions
    |> List.map ~f:Pasta_bindings.Fp.to_string ) ;
  assert (
    Pickles_types.Vector.Vector_5.equal Pasta_bindings.Fp.equal actions
      [ Pasta_bindings.Fp.of_string
          "27078409473756655551477733610137394255970846757871033008217988809531431111245"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ] ) ;
  mk_cmd (pay_fee 0 ()) [ Tree (submit_action, []) ]
  |> ledger_apply ~env ~global_slot ~state_view l ;
  let actions =
    (Option.value_exn (get_account l fee_payer).zkapp).action_state
  in
  Format.printf "%a\n\n" p
    ( Pickles_types.Vector.to_list actions
    |> List.map ~f:Pasta_bindings.Fp.to_string ) ;
  assert (
    Pickles_types.Vector.Vector_5.equal Pasta_bindings.Fp.equal actions
      [ Pasta_bindings.Fp.of_string
          "27124406866644488233811120381840879137649955857418976111208269224468943095218"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ] ) ;
  set := Account_id.Set.empty (* reset set, e.g. we're committing *) ;
  mk_cmd (pay_fee 0 ()) [ Tree (submit_action, []) ]
  |> ledger_apply ~env ~global_slot ~state_view l ;
  let actions =
    (Option.value_exn (get_account l fee_payer).zkapp).action_state
  in
  Format.printf "%a\n\n" p
    ( Pickles_types.Vector.to_list actions
    |> List.map ~f:Pasta_bindings.Fp.to_string ) ;
  assert (
    Pickles_types.Vector.Vector_5.equal Pasta_bindings.Fp.equal actions
      [ Pasta_bindings.Fp.of_string
          "9959670939348181654075053024715410609337071658342480266924089865823721510695"
      ; Pasta_bindings.Fp.of_string
          "27124406866644488233811120381840879137649955857418976111208269224468943095218"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ; Pasta_bindings.Fp.of_string
          "25079927036070901246064867767436987657692091363973573142121686150614948079097"
      ] ) ;
  ()

let () = main ()
