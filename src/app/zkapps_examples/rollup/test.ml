[@@@warning "-8-4"] (* ignore partial match and fragile-match warning *)

open Core
open Mina_ledger
open Currency
open Signature_lib
module U = Transaction_snark_tests.Util
open Mina_base
module For_tests = Mina_transaction_logic.For_tests
open Async_kernel
module Field = Snark_params.Tick.Field

type call_forest = Zeko_util.call_forest

type call_forest_tree = Zeko_util.call_forest_tree

let wait d = Async_unix.Thread_safe.block_on_async_exn d

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let depth = constraint_constants.ledger_depth

let outer_state_view = Mina_state.Protocol_state.Body.view U.genesis_state_body

let outer_global_slot = outer_state_view.global_slot_since_genesis

let zero_fee_payer (nonce : int) (kp : Keypair.t) : Account_update.Fee_payer.t =
  { body =
      { public_key = Public_key.compress kp.public_key
      ; fee = Fee.zero
      ; valid_until = None
      ; nonce = Account.Nonce.of_int nonce
      }
  ; authorization = Signature.dummy
  }

let outer_fee_payer = ref None

let pay_outer_fee =
  let nonce = ref 0 in
  fun () ->
    let r = zero_fee_payer !nonce (Option.value_exn !outer_fee_payer) in
    nonce := !nonce + 1 ;
    r

let inner_fee_payer : Account_update.Fee_payer.t =
  { body =
      { public_key = Public_key.Compressed.empty
      ; fee = Fee.zero
      ; valid_until = None
      ; nonce = Account.Nonce.zero
      }
  ; authorization = Signature.dummy
  }

let zeko_module =
  lazy
    (let module T = (val force U.snark_module) in
    (module Zkapps_rollup.Make (T) : Zkapps_rollup.S) )

let num_accounts = 4

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

let with_mask (type a) (ledger : Ledger.t) ~(f : Ledger.t -> a) : a =
  let mask = Ledger.Mask.create ~depth:(Ledger.depth ledger) () in
  let ledger' = Ledger.register_mask ledger mask in
  let r = f ledger' in
  let (_ : Ledger.unattached_mask) =
    Ledger.unregister_mask_exn ~loc:__LOC__ ledger'
  in
  r

let to_sparse ledger cmd =
  Sparse_ledger.of_ledger_subset_exn ledger
    (Zkapp_command.accounts_referenced cmd)

let take_from_sparse ledger sparse =
  Sparse_ledger.iteri sparse ~f:(fun i a -> Ledger.set_at_index_exn ledger i a) ;
  assert (
    Field.equal (Ledger.merkle_root ledger) (Sparse_ledger.merkle_root sparse) )

type staged_ledger = { ledger : Ledger.t; proof : Zkapps_rollup.t option ref }

(* FIXME: allow paying fees *)
let prove_zeko_command (staged_ledger : staged_ledger) cmd : unit =
  let fee_excess = Amount.Signed.zero in
  let ledger = to_sparse staged_ledger.ledger cmd in
  let state_body = Zkapps_rollup.inner_state_body in
  let state_view = Mina_state.Protocol_state.Body.view state_body in
  let global_slot = state_view.global_slot_since_genesis in
  let supply_increase = Amount.Signed.zero in
  let connecting_ledger, new_ledger =
    let partial_txn, states =
      Or_error.ok_exn
      @@ Sparse_ledger.apply_zkapp_first_pass_unchecked_with_states
           ~constraint_constants ~state_view ~global_slot ~fee_excess
           ~supply_increase ~first_pass_ledger:ledger ~second_pass_ledger:ledger
           cmd
    in
    let txn, states =
      Sparse_ledger.apply_zkapp_second_pass_unchecked_with_states ~init:states
        ledger partial_txn
      |> Or_error.ok_exn
    in
    check_no_failure txn cmd ;
    let last_global, _ = List.last_exn states in
    (last_global.first_pass_ledger, last_global.second_pass_ledger)
  in
  let module T = (val force U.snark_module) in
  let module Z = (val force zeko_module) in
  let init_stack = Mina_base.Pending_coinbase.Stack.empty in
  let pending_coinbase_state_stack :
      Transaction_snark.Pending_coinbase_stack_state.t =
    { source = Zkapps_rollup.inner_pending_coinbase
    ; target = Zkapps_rollup.inner_pending_coinbase
    }
  in
  let witnesses =
    Transaction_snark.zkapp_command_witnesses_exn ~constraint_constants
      ~global_slot ~state_body ~fee_excess
      [ ( `Pending_coinbase_init_stack init_stack
        , `Pending_coinbase_of_statement pending_coinbase_state_stack
        , `Sparse_ledger ledger
        , `Sparse_ledger connecting_ledger
        , `Connecting_ledger_hash (Sparse_ledger.merkle_root connecting_ledger)
        , cmd )
      ]
  in
  let open Async.Deferred.Let_syntax in
  (* FIXME: Do merging tree-style *)
  let stmt =
    wait (fun () ->
        match List.rev witnesses with
        | [] ->
            failwith "no witnesses generated"
        | (witness, spec, statement) :: rest ->
            let%bind p1 =
              printf "Proving first\n" ;
              T.of_zkapp_command_segment_exn ~statement ~witness ~spec
            in
            Async.Deferred.List.foldi ~init:p1 rest
              ~f:(fun i prev (witness, spec, statement) ->
                printf "Proving %ith\n" (i + 1) ;
                let%bind curr =
                  T.of_zkapp_command_segment_exn ~statement ~witness ~spec
                in
                let sok_digest =
                  Sok_message.create ~fee:Fee.one
                    ~prover:Public_key.Compressed.empty
                  |> Sok_message.digest
                in
                let%map merged = T.merge ~sok_digest prev curr in
                Or_error.ok_exn merged ) )
  in
  let stmt = wait (fun () -> Z.Wrapper.wrap stmt) in
  take_from_sparse staged_ledger.ledger new_ledger ;
  let proof =
    match !(staged_ledger.proof) with
    | Some proof ->
        wait (fun () -> Z.Wrapper.merge proof stmt)
    | None ->
        stmt
  in
  staged_ledger.proof := Some proof ;
  printf "prove_zeko_command done\n" ;
  ()

(* Only supports use_full_commitment for simplicity *)
let sign_cmd (cmd : Zkapp_command.t) (keys : Keypair.t list) : Zkapp_command.t =
  let keys = Option.value_exn !outer_fee_payer :: keys in
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete
      (Zkapp_command.commitment cmd)
      ~memo_hash:(Signed_command_memo.hash cmd.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create
           (Account_update.of_fee_payer cmd.fee_payer) )
  in
  let sign_raw (pk : Public_key.Compressed.t) msg =
    printf "Signing for %s\n" (Public_key.Compressed.to_base58_check pk) ;
    let rec go (keys : Keypair.t list) msg =
      match keys with
      | (kp : Keypair.t) :: keys ->
          if Public_key.Compressed.equal (Public_key.compress kp.public_key) pk
          then (
            printf "key found\n" ;
            Signature_lib.Schnorr.Chunked.sign
              ~signature_kind:Mina_signature_kind.Testnet kp.private_key
              (Random_oracle.Input.Chunked.field msg) )
          else (
            printf "not equal to %s\n"
              Public_key.(
                kp.public_key |> compress |> Compressed.to_base58_check) ;
            go keys msg )
      | [] ->
          failwithf "key not found: %s\n"
            (Public_key.Compressed.to_base58_check pk)
            ()
    in
    go keys msg
  in
  let rec sign_tree (tree : call_forest_tree) : call_forest_tree =
    { tree with
      account_update =
        { tree.account_update with
          authorization =
            ( match tree.account_update.body.authorization_kind with
            | Signature ->
                assert tree.account_update.body.use_full_commitment ;
                Signature
                  (sign_raw tree.account_update.body.public_key full_commitment)
            | _ ->
                tree.account_update.authorization )
        }
    ; calls = sign_forest tree.calls
    }
  and sign_forest (forest : call_forest) : call_forest =
    List.map ~f:(fun tree -> { tree with elt = sign_tree tree.elt }) forest
  in

  { cmd with
    fee_payer =
      { cmd.fee_payer with
        authorization =
          ( if Public_key.Compressed.(equal empty cmd.fee_payer.body.public_key)
          then cmd.fee_payer.authorization
          else sign_raw cmd.fee_payer.body.public_key full_commitment )
      }
  ; account_updates = sign_forest cmd.account_updates
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

let create_deploy_inner ~(outer_ledger : Ledger.t) ~(zeko_kp : Keypair.t) () =
  let module Z = (val force zeko_module) in
  let inner_ledger = Ledger.create_ephemeral ~depth () in
  Ledger.create_new_account_exn inner_ledger Z.Inner.account_id
    Z.Inner.initial_account ;
  let deploy_update = Z.Outer.deploy_exn inner_ledger in
  let deploy_update : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress zeko_kp.public_key
        ; update = deploy_update
        ; authorization_kind = Signature
        ; use_full_commitment = true
        ; implicit_account_creation_fee = false
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let deploy_cmd : Zkapp_command.t =
    { fee_payer = pay_outer_fee ()
    ; account_updates =
        Zkapp_command.Call_forest.(
          accumulate_hashes'
          @@ of_account_updates
               ~account_update_depth:(fun _ -> 0)
               [ deploy_update ])
    ; memo = Signed_command_memo.empty
    }
  in
  ledger_apply ~global_slot:outer_global_slot ~state_view:outer_state_view
    outer_ledger deploy_cmd ;
  inner_ledger

let create_outer ~(init_ledger : For_tests.Init_ledger.t) ~(zeko_kp : Keypair.t)
    () =
  let outer_ledger = Ledger.create_ephemeral ~depth () in
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    init_ledger outer_ledger ;
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (Option.value_exn !outer_fee_payer, Int64.max_value) |]
    outer_ledger ;
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (zeko_kp, Int64.zero) |]
    outer_ledger ;
  outer_ledger

let gen_init ~num_accounts () : For_tests.Init_ledger.t Quickcheck.Generator.t =
  let tbl = Public_key.Compressed.Hash_set.create () in
  let open Quickcheck.Generator in
  let open Let_syntax in
  let rec go acc n =
    if n = 0 then return (Array.of_list acc)
    else
      let%bind kp =
        filter Keypair.gen ~f:(fun kp ->
            not (Hash_set.mem tbl (Public_key.compress kp.public_key)) )
      in
      let amount = Int64.max_value in
      Hash_set.add tbl (Public_key.compress kp.public_key) ;
      go ((kp, amount) :: acc) (n - 1)
  in
  go [] num_accounts

let submit_transfer ~(transfers : Zkapps_rollup.TR.t list ref) (kp : Keypair.t)
    (recipient : Keypair.t)
    ~(prover : Zkapps_rollup.TR.t -> call_forest_tree Deferred.t) amount
    ~(fee_payer : Account_update.Fee_payer.t) =
  let transfer : Zkapps_rollup.TR.t =
    { recipient = Public_key.compress recipient.public_key; amount }
  in
  let transfer_update = wait (fun () -> prover transfer) in
  let transferrer_update : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress kp.public_key
        ; balance_change = Amount.Signed.(negate @@ of_unsigned amount)
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let transfer_cmd : Zkapp_command.t =
    { fee_payer
    ; account_updates =
        Zkapp_command.Call_forest.(
          cons_tree transfer_update @@ accumulate_hashes'
          @@ of_account_updates
               ~account_update_depth:(fun _ -> 0)
               [ transferrer_update ])
    ; memo = Signed_command_memo.empty
    }
  in
  let transfer_cmd = sign_cmd transfer_cmd [ kp ] in
  transfers := transfer :: !transfers ;
  transfer_cmd

let submit_deposit ~(zeko_kp : Keypair.t)
    ~(deposits : Zkapps_rollup.TR.t list ref) (kp : Keypair.t)
    (recipient : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  let outer_public_key = Public_key.compress zeko_kp.public_key in
  submit_transfer ~transfers:deposits ~fee_payer:(pay_outer_fee ())
    ~prover:(fun deposit -> Z.Outer.submit_deposit ~outer_public_key ~deposit)
    kp recipient amount

let submit_withdrawal ~(withdrawals : Zkapps_rollup.TR.t list ref)
    (kp : Keypair.t) (recipient : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  submit_transfer ~transfers:withdrawals ~fee_payer:inner_fee_payer
    ~prover:(fun withdrawal -> Z.Inner.submit_withdrawal ~withdrawal)
    kp recipient amount

(* takes list of transfers from new to old, skips all transfers before pointer *)
let skip_transfers_before (transfers : Zkapps_rollup.TR.t list)
    (pointer : Field.t) =
  match
    List.fold_right transfers
      ~init:(`Hashing Zkapp_account.Actions.empty_state_element)
      ~f:(fun transfer -> function
      | `Hashing hash ->
          if Field.equal hash pointer then `Accumulating [ transfer ]
          else
            `Hashing
              (Zkapp_account.Actions.push_events hash
                 (Zkapps_rollup.TR.to_actions transfer) )
      | `Accumulating transfers ->
          `Accumulating (transfer :: transfers) )
  with
  | `Hashing _ ->
      []
  | `Accumulating transfers_after_pointer ->
      transfers_after_pointer

let process_transfer ~is_new ~(ledger : Ledger.t)
    ~(transfers : Zkapps_rollup.TR.t list ref) ~(recipient : Keypair.t) amount
    ~(account_id : Account_id.t) ~(fee_payer : Account_update.Fee_payer.t)
    ~(prover :
          is_new:bool
       -> pointer:Field.t
       -> after:Zkapps_rollup.TR.t list
       -> before:Zkapps_rollup.TR.t list
       -> Zkapps_rollup.TR.t
       -> ([ `Pointer of Field.t ] * call_forest) Deferred.t ) : Zkapp_command.t
    =
  let token_id = Account_id.derive_token_id ~owner:account_id in
  let pointer =
    if is_new then Zkapp_account.Actions.empty_state_element
    else
      let (`Transfers_processed pointer) =
        get_account' ledger
          (Account_id.create
             (Public_key.compress recipient.public_key)
             token_id )
        |> Zkapps_rollup.read_token_account_state
      in
      pointer
  in
  let transfers_after_pointer = skip_transfers_before !transfers pointer in
  let (`After (before, after)
        : [ `After of Zkapps_rollup.TR.t list * Zkapps_rollup.TR.t list
          | `Before of Zkapps_rollup.TR.t list ] ) =
    List.fold_right transfers_after_pointer ~init:(`Before [])
      ~f:(fun transfer -> function
      | `Before before ->
          if
            Public_key.(
              Compressed.equal
                (compress recipient.public_key)
                transfer.recipient)
            && Currency.Amount.equal amount transfer.amount
          then `After (before, [])
          else `Before (transfer :: before)
      | `After (before, after) ->
          `After (before, transfer :: after) )
  in
  let `Pointer _, process_updates =
    wait (fun () ->
        prover ~is_new ~pointer ~after ~before
          { amount; recipient = Public_key.compress recipient.public_key } )
  in
  let process_cmd : Zkapp_command.t =
    { fee_payer
    ; account_updates = process_updates
    ; memo = Signed_command_memo.empty
    }
  in
  let process_cmd = sign_cmd process_cmd [ recipient ] in
  process_cmd

let process_deposit ~is_new ~staged_ledger ~deposits ~recipient amount =
  let module Z = (val force zeko_module) in
  let cmd =
    process_transfer ~is_new ~ledger:staged_ledger.ledger ~transfers:deposits
      ~recipient amount ~account_id:Zkapps_rollup.inner_account_id
      ~fee_payer:inner_fee_payer
      ~prover:(fun ~is_new ~pointer ~after ~before deposit ->
        Z.Inner.process_deposit ~is_new ~pointer ~after ~before ~deposit )
  in
  prove_zeko_command staged_ledger cmd ;
  ()

let process_withdrawal ~is_new ~outer_ledger ~withdrawals ~recipient
    ~(zeko_kp : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  let cmd =
    process_transfer ~is_new ~ledger:outer_ledger ~transfers:withdrawals
      ~recipient amount
      ~account_id:(Account_id.of_public_key zeko_kp.public_key)
      ~fee_payer:(pay_outer_fee ())
      ~prover:(fun ~is_new ~pointer ~after ~before withdrawal ->
        Z.Outer.process_withdrawal
          ~outer_public_key:(Public_key.compress zeko_kp.public_key)
          ~is_new ~pointer ~after ~before ~withdrawal )
  in
  ledger_apply ~global_slot:outer_global_slot ~state_view:outer_state_view
    outer_ledger cmd ;
  ()

let commit ~zeko_kp ~outer_ledger ~inner_ledger ~(staged_ledger : staged_ledger)
    ~(deposits : Zkapps_rollup.TR.t list ref) =
  let module Z = (val force zeko_module) in
  let (`All_deposits old_all_deposits) =
    get_account' inner_ledger Zkapps_rollup.inner_account_id
    |> Zkapps_rollup.read_inner_state
  in
  let (all_deposits :: _) =
    (Option.value_exn (get_account outer_ledger zeko_kp).zkapp).action_state
  in
  let inner_step_update = wait (fun () -> Z.Inner.step ~all_deposits) in
  let inner_step_cmd : Zkapp_command.t =
    { fee_payer = inner_fee_payer
    ; account_updates =
        Zkapp_command.Call_forest.(cons_tree inner_step_update [])
    ; memo = Signed_command_memo.empty
    }
  in
  prove_zeko_command staged_ledger inner_step_cmd ;
  let new_deposits = skip_transfers_before !deposits old_all_deposits in
  let outer_step_update =
    wait (fun () ->
        Z.Outer.step
          (Option.value_exn !(staged_ledger.proof))
          ~outer_public_key:(Public_key.compress zeko_kp.public_key)
          ~new_deposits
          ~new_inner_ledger:(to_sparse staged_ledger.ledger inner_step_cmd)
          ~old_inner_ledger:(to_sparse inner_ledger inner_step_cmd) )
  in
  let outer_step_cmd : Zkapp_command.t =
    { fee_payer = pay_outer_fee ()
    ; account_updates =
        Zkapp_command.Call_forest.(cons_tree outer_step_update [])
    ; memo = Signed_command_memo.empty
    }
  in
  ledger_apply ~global_slot:outer_global_slot ~state_view:outer_state_view
    outer_ledger outer_step_cmd ;
  Ledger.commit staged_ledger.ledger ;
  staged_ledger.proof := None ;
  ()

(* FIXME: make shrinkable by making it pure
   and taking all random inputs as a spec.
*)
let main () =
  let zeko_kp = Signature_lib.Keypair.create () in
  let init_ledger = Quickcheck.random_value (gen_init ~num_accounts ()) in
  outer_fee_payer := Some (Quickcheck.random_value Keypair.gen) ;
  let outer_ledger = create_outer ~init_ledger ~zeko_kp () in
  let inner_ledger = create_deploy_inner ~outer_ledger ~zeko_kp () in
  with_mask inner_ledger ~f:(fun staged_ledger_ledger ->
      let staged_ledger = { ledger = staged_ledger_ledger; proof = ref None } in
      let deposits = ref [] in
      let withdrawals = ref [] in
      let outer_apply =
        ledger_apply ~global_slot:outer_global_slot ~state_view:outer_state_view
          outer_ledger
      in
      for i = 1 to 2 do
        printf "i == %i\n" i ;
        let is_new = Int.equal i 1 in
        Array.iter init_ledger ~f:(fun (kp, amount) ->
            let amount =
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 4))
            in
            outer_apply (submit_deposit ~zeko_kp ~deposits kp kp amount) ;
            () ) ;
        commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
        Array.iter init_ledger ~f:(fun (kp, amount) ->
            let amount =
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 4))
            in
            process_deposit ~is_new ~staged_ledger ~deposits ~recipient:kp
              amount ;
            () ) ;
        commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
        Array.iter init_ledger ~f:(fun (kp, amount) ->
            let amount =
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 8))
            in
            prove_zeko_command staged_ledger
              (submit_withdrawal ~withdrawals kp kp amount) ;
            () ) ;
        commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
        Array.iter init_ledger ~f:(fun (kp, amount) ->
            let amount =
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 8))
            in
            process_withdrawal ~zeko_kp ~is_new ~outer_ledger ~withdrawals
              ~recipient:kp amount ;
            () ) ;
        ()
      done )

let () = main ()
