module Util_ = Util
open Core
open Mina_ledger
open Currency
open Signature_lib
open Mina_transaction
module U = Transaction_snark_tests.Util
open Mina_base
module For_tests = Mina_transaction_logic.For_tests
open Async_kernel
module Field = Snark_params.Tick.Field
module Util = Util_

let wait d = Async_unix.Thread_safe.block_on_async_exn d

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let depth = constraint_constants.ledger_depth

let zero_fee_payer (nonce : int) (kp : Keypair.t) : Account_update.Fee_payer.t =
  { body =
      { public_key = Public_key.compress kp.public_key
      ; fee = Fee.zero
      ; valid_until = None
      ; nonce = Account.Nonce.of_int nonce
      }
  ; authorization = Signature.dummy
  }

let dummy_fee_payer : Account_update.Fee_payer.t =
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
    (let module T = (val Lazy.force U.snark_module) in
    (module Zkapps_rollup.Make (T) : Zkapps_rollup.S) )

let num_transactions = 2

type call_forest =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.t

type call_forest_tree =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.Tree.t

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

let merkle_root_after_zkapp_command_first_pass_exn ~constraint_constants
    ~global_slot ~state_view ledger cmd =
  with_mask ledger ~f:(fun ledger ->
      let applied, () =
        Or_error.ok_exn
          (Ledger.apply_zkapp_command_first_pass_aux ~init:()
             ~f:(fun _ _ -> ())
             ~constraint_constants ~global_slot ~state_view ledger cmd )
      in
      Ledger.merkle_root ledger )

let to_sparse ledger cmd =
  Sparse_ledger.of_ledger_subset_exn ledger
    (Zkapp_command.accounts_referenced cmd)

let take_from_sparse ledger sparse =
  Sparse_ledger.iteri sparse ~f:(fun i a -> Ledger.set_at_index_exn ledger i a) ;
  assert (
    Field.equal (Ledger.merkle_root ledger) (Sparse_ledger.merkle_root sparse) )

(* Adapted from Transaction_snark_tests.Util.check_zkapp_command_with_merges_exn *)
(* FIXME: fix support for fee_excess that isn't 0 *)
let prove_zeko_command ~fee_excess ledger cmd :
    (Zkapps_rollup.t * Sparse_ledger.t) Deferred.t =
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
  let module T = (val Lazy.force U.snark_module) in
  let module Z = (val Lazy.force zeko_module) in
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
  let%bind stmt =
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
            Or_error.ok_exn merged )
  in
  let%map stmt = Z.Wrapper.wrap stmt in
  printf "prove_zeko_command done\n" ;
  (stmt, new_ledger)

(* Only supports use_full_commitment for simplicity *)
let sign_cmd (cmd : Zkapp_command.t) (keys : Keypair.t list) : Zkapp_command.t =
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

let dummy_update : Account_update.Body.t =
  { Account_update.Body.dummy with use_full_commitment = true }

let get_account ledger (kp : Keypair.t) =
  let account_id = Account_id.of_public_key kp.public_key in
  let (`Existed : [ `Added | `Existed ]), loc =
    Or_error.ok_exn
    @@ Ledger.get_or_create_account ledger account_id
         (Account.initialize account_id)
  in
  let account = Option.value_exn @@ Ledger.get ledger loc in
  account

(* Test to be done:
   Given a list of accounts and intra-ledger-transfers,
   deposit all of the funds over two inter-ledger-transfers
   per account, in a random order.
   Then do all intra-ledger-transfers,
   and withdraw all the funds back in the same random manner, two withdrawals
   per account.
*)
let main () =
  let module T = (val Lazy.force U.snark_module) in
  let module Z = (val Lazy.force zeko_module) in
  let zeko_kp = Signature_lib.Keypair.create () in
  let ({ init_ledger; specs } : For_tests.Test_spec.t) =
    Quickcheck.random_value (For_tests.Test_spec.mk_gen ~num_transactions ())
  in
  (* Adjust for account creation fee because we need to create
     the accounts again on the inner ledger *)
  let init_ledger =
    Array.map init_ledger ~f:(fun (kp, b) ->
        ( kp
        , Int64.(
            b
            + ( Unsigned.UInt64.to_int64
              @@ Fee.to_uint64 constraint_constants.account_creation_fee )) ) )
  in
  let outer_ledger =
    Ledger.create_ephemeral ~depth:constraint_constants.ledger_depth ()
  in
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    init_ledger outer_ledger ;
  let fee_payer_kp = Quickcheck.random_value Keypair.gen in
  let zero_fee_payer =
    let nonce = ref 0 in
    fun () ->
      let r = zero_fee_payer !nonce fee_payer_kp in
      nonce := !nonce + 1 ;
      r
  in
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (fee_payer_kp, Int64.max_value) |]
    outer_ledger ;
  For_tests.Init_ledger.init
    (module Ledger.Ledger_inner)
    [| (zeko_kp, Int64.zero) |]
    outer_ledger ;
  let inner_ledger =
    Ledger.create_ephemeral ~depth:constraint_constants.ledger_depth ()
  in
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
    { fee_payer = zero_fee_payer ()
    ; account_updates =
        Zkapp_command.Call_forest.(
          accumulate_hashes'
          @@ of_account_updates (fun _ -> 0) [ deploy_update ])
    ; memo = Signed_command_memo.empty
    }
  in
  let state_view = Mina_state.Protocol_state.Body.view U.genesis_state_body in
  let global_slot = state_view.global_slot_since_genesis in
  ledger_apply ~global_slot ~state_view outer_ledger deploy_cmd ;
  List.iter [ true ] ~f:(fun first_half ->
      Array.iter init_ledger ~f:(fun (kp, amount) ->
          let (old_all_deposits :: _) =
            (Option.value_exn (get_account outer_ledger zeko_kp).zkapp)
              .action_state
          in

          (* First time we take half, second time we take rest *)
          let amount =
            if first_half then
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 2))
            else
              Amount.of_uint64
              @@ Unsigned_extended.UInt64.(
                   sub (of_int64 amount) (div (of_int64 amount) (of_int 2)))
          in
          let deposit : Zkapps_rollup.TR.t =
            { amount; recipient = Public_key.compress kp.public_key }
          in
          (let deposit_update =
             wait (fun () ->
                 Z.Outer.submit_deposit
                   ~outer_public_key:(Public_key.compress zeko_kp.public_key)
                   ~deposit )
           in
           let depositer_update : Account_update.t =
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
           let deposit_cmd : Zkapp_command.t =
             { fee_payer = zero_fee_payer ()
             ; account_updates =
                 Zkapp_command.Call_forest.(
                   cons_tree deposit_update @@ accumulate_hashes'
                   @@ of_account_updates (fun _ -> 0) [ depositer_update ])
             ; memo = Signed_command_memo.empty
             }
           in
           ledger_apply ~global_slot ~state_view outer_ledger deposit_cmd ) ;

          let (all_deposits :: _) =
            (Option.value_exn (get_account outer_ledger zeko_kp).zkapp)
              .action_state
          in

          (let inner_step_update =
             wait (fun () -> Z.Inner.step ~all_deposits)
           in
           let inner_step_cmd : Zkapp_command.t =
             { fee_payer = dummy_fee_payer
             ; account_updates =
                 Zkapp_command.Call_forest.(cons_tree inner_step_update [])
             ; memo = Signed_command_memo.empty
             }
           in
           let inner_step_transition, new_inner_ledger =
             wait (fun () ->
                 prove_zeko_command ~fee_excess:Amount.Signed.zero
                   (to_sparse inner_ledger inner_step_cmd)
                   inner_step_cmd )
           in
           let outer_step_update =
             wait (fun () ->
                 Z.Outer.step inner_step_transition
                   ~outer_public_key:(Public_key.compress zeko_kp.public_key)
                   ~new_deposits:[ deposit ] ~new_inner_ledger
                   ~old_inner_ledger:(to_sparse inner_ledger inner_step_cmd) )
           in
           let outer_step_cmd : Zkapp_command.t =
             { fee_payer = zero_fee_payer ()
             ; account_updates =
                 Zkapp_command.Call_forest.(cons_tree outer_step_update [])
             ; memo = Signed_command_memo.empty
             }
           in
           ledger_apply ~global_slot ~state_view outer_ledger outer_step_cmd ;

           take_from_sparse inner_ledger new_inner_ledger ) ;

          let pointer =
            if first_half then Some old_all_deposits
            else
              let account_id = Account_id.of_public_key kp.public_key in
              let (`Existed : [ `Added | `Existed ]), loc =
                Or_error.ok_exn
                @@ Ledger.get_or_create_account inner_ledger account_id
                     (Account.initialize account_id)
              in
              let account = Option.value_exn @@ Ledger.get inner_ledger loc in
              let (Some zkapp) = account.zkapp in
              let (transfers_processed :: _) = zkapp.app_state in
              Some transfers_processed
          in
          let `Pointer _, process_updates =
            wait (fun () ->
                Z.Inner.process_deposit ~is_new:first_half ~pointer ~after:[]
                  ~before:[] ~deposit )
          in
          let process_cmd : Zkapp_command.t =
            { fee_payer = dummy_fee_payer
            ; account_updates = process_updates
            ; memo = Signed_command_memo.empty
            }
          in
          let process_cmd = sign_cmd process_cmd [ kp ] in
          let process_transition, new_inner_ledger =
            wait (fun () ->
                prove_zeko_command ~fee_excess:Amount.Signed.zero
                  (to_sparse inner_ledger process_cmd)
                  process_cmd )
          in
          let outer_step_update =
            wait (fun () ->
                Z.Outer.step process_transition
                  ~outer_public_key:(Public_key.compress zeko_kp.public_key)
                  ~new_deposits:[] ~new_inner_ledger
                  ~old_inner_ledger:(to_sparse inner_ledger process_cmd) )
          in
          let outer_step_cmd : Zkapp_command.t =
            { fee_payer = zero_fee_payer ()
            ; account_updates =
                Zkapp_command.Call_forest.(cons_tree outer_step_update [])
            ; memo = Signed_command_memo.empty
            }
          in
          ledger_apply ~global_slot ~state_view outer_ledger outer_step_cmd ;
          take_from_sparse inner_ledger new_inner_ledger ) ) ;

  (*

  let cmds = List.fold ~f:(fun spec ->
    For_tests.account_update_send spec
    |> prove_zeko_command ~fee_excess:(Amount.(Signed.of_unsigned @@ of_fee spec.fee))
  ) specs in
  List.iter cmds ~f:(fun cmd ->
    
    ()
  ) ;

*)
  ()

let () = main ()
