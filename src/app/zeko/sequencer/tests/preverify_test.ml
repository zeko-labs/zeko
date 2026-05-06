open Core_kernel
open Async
open Mina_base
open Mina_numbers
open Currency
open Signature_lib
open Sequencer_lib
module L = Mina_ledger.Ledger

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Info ;
  Logger.create ()

let signature_kind = Mina_signature_kind.t_DEPRECATED

let proof_cache_db = Proof_cache_tag.For_tests.create_db ()

(* L2 constants — same as the sequencer uses. Account creation fee is 0.1 mina. *)
let l2_constraint_constants = Zeko_constants.constraint_constants

(* Simulated L1 constants — same shape, but with the larger 1 mina account
   creation fee that mainnet/devnet use. This is enough to exercise the
   "different constants" path through the preverify utility. *)
let l1_constraint_constants : Genesis_constants.Constraint_constants.t =
  { l2_constraint_constants with
    account_creation_fee = Currency.Fee.of_mina_string_exn "1.0"
  }

let consensus_constants =
  let protocol_constants : Genesis_constants.Protocol.t =
    { k = 1
    ; slots_per_epoch = 1000
    ; slots_per_sub_window = 1
    ; grace_period_slots = 1
    ; delta = 1
    ; genesis_state_timestamp = Int64.one
    }
  in
  Consensus.Constants.create ~constraint_constants:l2_constraint_constants
    ~protocol_constants

let compile_time_genesis =
  Mina_state.Genesis_protocol_state.t
    ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
    ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
    ~constraint_constants:l2_constraint_constants ~consensus_constants
    ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference

let state_view ~global_slot : Zkapp_precondition.Protocol_state.View.t =
  let s = Mina_state.Protocol_state.Body.view compile_time_genesis.data.body in
  { snarked_ledger_hash = s.snarked_ledger_hash
  ; blockchain_length = s.blockchain_length
  ; min_window_density = s.min_window_density
  ; total_currency = s.total_currency
  ; global_slot_since_genesis = global_slot
  ; staking_epoch_data = s.staking_epoch_data
  ; next_epoch_data = s.next_epoch_data
  }

let global_slot = Global_slot_since_genesis.zero

(* Build a fully funded test ledger with two accounts. *)
let make_ledger ~depth (accounts : (Account_id.t * Account.t) list) : L.t =
  let ledger = L.create_ephemeral ~depth () in
  List.iter accounts ~f:(fun (aid, acc) -> L.create_new_account_exn ledger aid acc) ;
  ledger

let funded_account aid balance : Account.t =
  let acc = Account.create aid (Balance.of_mina_int_exn balance) in
  acc

let make_payment ~sender ~receiver ~amount ~fee ~nonce : Signed_command.t =
  let sender_pk = Public_key.compress sender.Keypair.public_key in
  let payload : Signed_command.Payload.t =
    Signed_command.Payload.create ~fee ~fee_payer_pk:sender_pk ~nonce
      ~memo:Signed_command_memo.dummy ~valid_until:None
      ~body:(Payment { receiver_pk = receiver; amount })
  in
  Signed_command.sign ~signature_kind sender payload
  |> Signed_command.forget_check

let make_zkapp_send ~sender ~receiver ~amount ~fee ~nonce : Zkapp_command.t =
  let sender_pk = Public_key.compress sender.Keypair.public_key in
  let zkapp_command : Zkapp_command.Simple.t =
    { fee_payer =
        Account_update.Fee_payer.make
          ~body:
            { public_key = sender_pk; fee; valid_until = None; nonce }
          ~authorization:Signature.dummy
    ; account_updates =
        [ Account_update.with_no_aux
            ~body:
              { Account_update.Body.Simple.public_key = sender_pk
              ; update = Account_update.Update.noop
              ; token_id = Token_id.default
              ; balance_change = Amount.Signed.(negate (of_unsigned amount))
              ; increment_nonce = true
              ; events = []
              ; actions = []
              ; call_data = Snark_params.Tick.Field.zero
              ; call_depth = 0
              ; preconditions =
                  { network = Zkapp_precondition.Protocol_state.accept
                  ; account = Zkapp_precondition.Account.accept
                  ; valid_while = Zkapp_basic.Or_ignore.Ignore
                  }
              ; may_use_token = No
              ; use_full_commitment = true
              ; implicit_account_creation_fee = true
              ; authorization_kind = Signature
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        ; Account_update.with_no_aux
            ~body:
              { Account_update.Body.Simple.public_key = receiver
              ; update = Account_update.Update.noop
              ; token_id = Token_id.default
              ; balance_change = Amount.Signed.of_unsigned amount
              ; increment_nonce = false
              ; events = []
              ; actions = []
              ; call_data = Snark_params.Tick.Field.zero
              ; call_depth = 0
              ; preconditions =
                  { network = Zkapp_precondition.Protocol_state.accept
                  ; account = Zkapp_precondition.Account.accept
                  ; valid_while = Zkapp_basic.Or_ignore.Ignore
                  }
              ; may_use_token = No
              ; use_full_commitment = false
              ; implicit_account_creation_fee = true
              ; authorization_kind = None_given
              }
            ~authorization:Control.Poly.None_given
        ]
    ; memo = Signed_command_memo.empty
    }
  in
  Zkapp_command.of_simple ~signature_kind ~proof_cache_db zkapp_command

(* Get-account function that reads from a local in-memory ledger. Models the
   L2 path: the sequencer holds the canonical ledger locally and answers
   synchronously. *)
let get_account_from_ledger ledger : Account_id.t -> Account.t option Deferred.Or_error.t
    =
 fun aid ->
  let result =
    let%bind.Option loc = L.location_of_account ledger aid in
    L.get ledger loc
  in
  Deferred.Or_error.return result

(* Get-account function that reads from a precomputed table. Models the
   L1 path: the caller has fetched accounts via GraphQL and stuffed them in a
   table. The point of this test isn't network round-trips, just that the
   preverify function does not care where accounts come from. *)
let get_account_from_table table : Account_id.t -> Account.t option Deferred.Or_error.t =
 fun aid -> Deferred.Or_error.return (Hashtbl.find table aid)

let assert_ok ~msg = function
  | Ok () ->
      ()
  | Error err ->
      failwithf "%s: %s" msg (Error.to_string_hum err) ()

let assert_error ~msg = function
  | Ok () ->
      failwithf "expected error but got Ok: %s" msg ()
  | Error _ ->
      ()

let run = Thread_safe.block_on_async_exn

let () = print_endline "(* preverify: L2 — successful payment via local ledger *)"

let () =
  let sender_kp = Keypair.create () in
  let receiver_kp = Keypair.create () in
  let sender_pk = Public_key.compress sender_kp.public_key in
  let receiver_pk = Public_key.compress receiver_kp.public_key in
  let sender_aid = Account_id.create sender_pk Token_id.default in
  let receiver_aid = Account_id.create receiver_pk Token_id.default in
  let ledger =
    make_ledger ~depth:l2_constraint_constants.ledger_depth
      [ (sender_aid, funded_account sender_aid 1000)
      ; (receiver_aid, funded_account receiver_aid 0)
      ]
  in
  let payment =
    make_payment ~sender:sender_kp ~receiver:receiver_pk
      ~amount:(Amount.of_mina_int_exn 5)
      ~fee:(Fee.of_mina_int_exn 1) ~nonce:Account_nonce.zero
  in
  let result =
    run (fun () ->
        Zeko_transaction_logic.preverify_user_command
          ~get_account:(get_account_from_ledger ledger)
          ~constraint_constants:l2_constraint_constants ~global_slot
          ~state_view:(state_view ~global_slot)
          (User_command.Signed_command payment) )
  in
  assert_ok ~msg:"L2 payment should succeed" result ;
  [%log info] "OK: L2 payment preverified"

let () =
  print_endline
    "(* preverify: L1 — same payment via fetched-account table, larger \
     account_creation_fee *)"

let () =
  let sender_kp = Keypair.create () in
  let receiver_kp = Keypair.create () in
  let sender_pk = Public_key.compress sender_kp.public_key in
  let receiver_pk = Public_key.compress receiver_kp.public_key in
  let sender_aid = Account_id.create sender_pk Token_id.default in
  let receiver_aid = Account_id.create receiver_pk Token_id.default in
  let table = Hashtbl.create (module Account_id) in
  Hashtbl.set table ~key:sender_aid ~data:(funded_account sender_aid 1000) ;
  Hashtbl.set table ~key:receiver_aid ~data:(funded_account receiver_aid 0) ;
  let payment =
    make_payment ~sender:sender_kp ~receiver:receiver_pk
      ~amount:(Amount.of_mina_int_exn 5)
      ~fee:(Fee.of_mina_int_exn 2) ~nonce:Account_nonce.zero
  in
  let result =
    run (fun () ->
        Zeko_transaction_logic.preverify_user_command
          ~get_account:(get_account_from_table table)
          ~constraint_constants:l1_constraint_constants ~global_slot
          ~state_view:(state_view ~global_slot)
          (User_command.Signed_command payment) )
  in
  assert_ok ~msg:"L1 payment should succeed" result ;
  [%log info] "OK: L1 payment preverified"

let () =
  print_endline
    "(* preverify: insufficient balance — payment should fail at preverify *)"

let () =
  let sender_kp = Keypair.create () in
  let receiver_kp = Keypair.create () in
  let sender_pk = Public_key.compress sender_kp.public_key in
  let receiver_pk = Public_key.compress receiver_kp.public_key in
  let sender_aid = Account_id.create sender_pk Token_id.default in
  let receiver_aid = Account_id.create receiver_pk Token_id.default in
  let ledger =
    make_ledger ~depth:l2_constraint_constants.ledger_depth
      [ (sender_aid, funded_account sender_aid 1)
      ; (receiver_aid, funded_account receiver_aid 0)
      ]
  in
  let payment =
    make_payment ~sender:sender_kp ~receiver:receiver_pk
      ~amount:(Amount.of_mina_int_exn 100)
      ~fee:(Fee.of_mina_int_exn 1) ~nonce:Account_nonce.zero
  in
  let result =
    run (fun () ->
        Zeko_transaction_logic.preverify_user_command
          ~get_account:(get_account_from_ledger ledger)
          ~constraint_constants:l2_constraint_constants ~global_slot
          ~state_view:(state_view ~global_slot)
          (User_command.Signed_command payment) )
  in
  assert_error ~msg:"insufficient-balance payment should fail at preverify"
    result ;
  [%log info] "OK: underfunded payment caught"

let () =
  print_endline
    "(* preverify: L2 — successful zkapp_command (no real signatures) *)"

let () =
  let sender_kp = Keypair.create () in
  let receiver_kp = Keypair.create () in
  let sender_pk = Public_key.compress sender_kp.public_key in
  let receiver_pk = Public_key.compress receiver_kp.public_key in
  let sender_aid = Account_id.create sender_pk Token_id.default in
  let receiver_aid = Account_id.create receiver_pk Token_id.default in
  let ledger =
    make_ledger ~depth:l2_constraint_constants.ledger_depth
      [ (sender_aid, funded_account sender_aid 1000)
      ; (receiver_aid, funded_account receiver_aid 0)
      ]
  in
  let cmd =
    make_zkapp_send ~sender:sender_kp ~receiver:receiver_pk
      ~amount:(Amount.of_mina_int_exn 5)
      ~fee:(Fee.of_mina_int_exn 1) ~nonce:Account_nonce.zero
  in
  let result =
    run (fun () ->
        Zeko_transaction_logic.preverify_user_command
          ~get_account:(get_account_from_ledger ledger)
          ~constraint_constants:l2_constraint_constants ~global_slot
          ~state_view:(state_view ~global_slot)
          (User_command.Zkapp_command cmd) )
  in
  assert_ok ~msg:"L2 zkapp_command should succeed" result ;
  [%log info] "OK: L2 zkapp_command preverified"

let () =
  print_endline
    "(* preverify: L1 — same zkapp_command via fetched-account table *)"

let () =
  let sender_kp = Keypair.create () in
  let receiver_kp = Keypair.create () in
  let sender_pk = Public_key.compress sender_kp.public_key in
  let receiver_pk = Public_key.compress receiver_kp.public_key in
  let sender_aid = Account_id.create sender_pk Token_id.default in
  let receiver_aid = Account_id.create receiver_pk Token_id.default in
  let table = Hashtbl.create (module Account_id) in
  Hashtbl.set table ~key:sender_aid ~data:(funded_account sender_aid 1000) ;
  Hashtbl.set table ~key:receiver_aid ~data:(funded_account receiver_aid 0) ;
  let cmd =
    make_zkapp_send ~sender:sender_kp ~receiver:receiver_pk
      ~amount:(Amount.of_mina_int_exn 5)
      ~fee:(Fee.of_mina_int_exn 2) ~nonce:Account_nonce.zero
  in
  let result =
    run (fun () ->
        Zeko_transaction_logic.preverify_user_command
          ~get_account:(get_account_from_table table)
          ~constraint_constants:l1_constraint_constants ~global_slot
          ~state_view:(state_view ~global_slot)
          (User_command.Zkapp_command cmd) )
  in
  assert_ok ~msg:"L1 zkapp_command should succeed" result ;
  [%log info] "OK: L1 zkapp_command preverified"

let () = print_endline "All preverify tests passed."
