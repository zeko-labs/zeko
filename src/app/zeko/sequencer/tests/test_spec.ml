open Core_kernel
open Async
open Mina_base
open Mina_numbers
open Currency
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Sequencer
open Zeko_types

let run = Thread_safe.block_on_async_exn

module Account_without_receipt_chain_hash = struct
  type t =
    ( Public_key.Compressed.t
    , Token_id.t
    , Account.Token_symbol.t
    , Balance.t
    , Account_nonce.t
    , unit
    , Public_key.Compressed.t option
    , State_hash.t
    , Account_timing.t
    , Permissions.t
    , Zkapp_account.t option )
    Account.Poly.t
  [@@deriving sexp, compare]
end

let min_init_balance = Int64.of_string "8000000000"

let max_init_balance = Int64.of_string "8000000000000"

let num_accounts = 10

let proof_cache_db = Proof_cache_tag.For_tests.create_db ()

module Init_ledger = struct
  type t = (Keypair.t * int64) array [@@deriving sexp]

  let init ?(zkapp = true) (type l) (module L : Ledger_intf.S with type t = l)
      (init_ledger : t) (l : L.t) =
    Array.iter init_ledger ~f:(fun (kp, amount) ->
        let _tag, account, loc =
          L.get_or_create l
            (Account_id.create
               (Public_key.compress kp.public_key)
               Token_id.default )
          |> Or_error.ok_exn
        in
        let permissions : Permissions.t =
          { edit_state = Either
          ; send = Either
          ; receive = None
          ; set_delegate = Either
          ; set_permissions = Either
          ; set_verification_key = (Either, Mina_numbers.Txn_version.current)
          ; set_zkapp_uri = Either
          ; edit_action_state = Either
          ; set_token_symbol = Either
          ; increment_nonce = Either
          ; set_voting_for = Either
          ; access = None
          ; set_timing = Either
          }
        in
        let zkapp =
          if zkapp then
            Some
              { Zkapp_account.default with
                verification_key =
                  Some
                    { With_hash.hash = Zkapp_basic.F.zero
                    ; data = Side_loaded_verification_key.dummy
                    }
              }
          else None
        in
        L.set l loc
          { account with
            balance =
              Currency.Balance.of_uint64 (Unsigned.UInt64.of_int64 amount)
          ; permissions
          ; zkapp
          } )

  let gen () : t Quickcheck.Generator.t =
    let tbl = Public_key.Compressed.Hash_set.create () in
    let open Quickcheck.Generator in
    let open Let_syntax in
    let rec go acc n =
      if n = 0 then return (Array.of_list acc)
      else
        let%bind kp =
          filter Keypair.gen ~f:(fun kp ->
              not (Hash_set.mem tbl (Public_key.compress kp.public_key)) )
        and amount = Int64.gen_incl min_init_balance max_init_balance in
        Hash_set.add tbl (Public_key.compress kp.public_key) ;
        go ((kp, amount) :: acc) (n - 1)
    in
    go [] num_accounts
end

module Transaction_spec = struct
  type t =
    { fee : Currency.Fee.t
    ; sender : Keypair.t * Account_nonce.t
    ; receiver : Public_key.Compressed.t
    ; amount : Currency.Amount.t
    }
  [@@deriving sexp]

  let gen ~(init_ledger : Init_ledger.t) ~nonces =
    let pk ((kp : Keypair.t), _) = Public_key.compress kp.public_key in
    let open Quickcheck.Generator.Let_syntax in
    let%bind receiver_is_new = Bool.quickcheck_generator in
    let gen_index () = Int.gen_incl 0 (Array.length init_ledger - 1) in
    let%bind receiver_index =
      if receiver_is_new then return None else gen_index () >>| Option.return
    in
    let%bind receiver =
      match receiver_index with
      | None ->
          Public_key.Compressed.gen
      | Some i ->
          return (pk init_ledger.(i))
    in
    let%bind sender =
      let%map i =
        match receiver_index with
        | None ->
            gen_index ()
        | Some j ->
            Quickcheck.Generator.filter (gen_index ()) ~f:(( <> ) j)
      in
      fst init_ledger.(i)
    in
    let gen_amount () =
      Currency.Amount.(
        gen_incl
          (of_nanomina_int_exn 1_000_000_000)
          (of_nanomina_int_exn 100_000_000_000))
    in
    let gen_fee () =
      Currency.Fee.(
        gen_incl
          (of_nanomina_int_exn 1_000_000_000)
          (of_nanomina_int_exn 100_000_000_000))
    in
    let nonce : Account_nonce.t = Map.find_exn nonces sender in
    let%bind fee = gen_fee () in
    let%bind amount = gen_amount () in
    let nonces = Map.set nonces ~key:sender ~data:(Account_nonce.succ nonce) in
    let spec = { fee; amount; receiver; sender = (sender, nonce) } in
    return (spec, nonces)
end

module Test_spec = struct
  type t = { init_ledger : Init_ledger.t; specs : Transaction_spec.t list }
  [@@deriving sexp]

  let mk_gen ~num_transactions () =
    let open Quickcheck.Generator.Let_syntax in
    let%bind init_ledger = Init_ledger.gen () in
    let%bind specs =
      let rec go acc n nonces =
        if n = 0 then return (List.rev acc)
        else
          let%bind spec, nonces = Transaction_spec.gen ~init_ledger ~nonces in
          go (spec :: acc) (n - 1) nonces
      in
      go [] num_transactions
        (Keypair.Map.of_alist_exn
           (List.map (Array.to_list init_ledger) ~f:(fun (pk, _) ->
                (pk, Account_nonce.zero) ) ) )
    in
    return { init_ledger; specs }
end

let command_send ?chain ?(valid_until = Global_slot_since_genesis.max_value)
    { Transaction_spec.fee; sender = sender, sender_nonce; receiver; amount } :
    Signed_command.t =
  let sender_pk = Public_key.compress sender.public_key in
  let signature_kind =
    match chain with
    | Some chain ->
        chain
    | _ ->
        Mina_signature_kind.t_DEPRECATED
  in
  Signed_command.sign ~signature_kind sender
    { common =
        { fee
        ; fee_payer_pk = sender_pk
        ; nonce = sender_nonce
        ; valid_until
        ; memo = Signed_command_memo.dummy
        }
    ; body = Payment { receiver_pk = receiver; amount }
    }
  |> Signed_command.forget_check

let account_update_send ?chain ?(use_full_commitment = true)
    ?(double_sender_nonce = true) ?(valid_until = None)
    ?(valid_while =
      (Zkapp_basic.Or_ignore.Ignore, Zkapp_basic.Or_ignore.Ignore))
    ?(global_slot_precondition =
      (Zkapp_basic.Or_ignore.Ignore, Zkapp_basic.Or_ignore.Ignore))
    { Transaction_spec.fee; sender = sender, sender_nonce; receiver; amount } :
    Zkapp_command.t =
  let signature_kind =
    Option.value ~default:Mina_signature_kind.t_DEPRECATED chain
  in
  let sender_pk = Public_key.compress sender.public_key in
  let actual_nonce =
    (* Here, we double the spec'd nonce, because we bump the nonce a second
       time for the 'sender' part of the payment.
    *)
    (* TODO: We should make bumping the nonce for signed zkapp_command optional,
       flagged by a field in the account_update (but always true for the fee payer).

       This would also allow us to prevent replays of snapp proofs, by
       allowing them to bump their nonce.
    *)
    if double_sender_nonce then
      sender_nonce |> Account.Nonce.to_uint32
      |> Unsigned.UInt32.(mul (of_int 2))
      |> Account.Nonce.to_uint32
    else sender_nonce
  in
  let zkapp_command : Zkapp_command.Simple.t =
    { fee_payer =
        (* Real signature added in below *)
        Account_update.Fee_payer.make
          ~body:
            { public_key = sender_pk; fee; valid_until; nonce = actual_nonce }
          ~authorization:Signature.dummy
    ; account_updates =
        [ Account_update.with_no_aux
            ~body:
              { Account_update.Body.Simple.public_key = sender_pk
              ; update = Account_update.Update.noop
              ; token_id = Token_id.default
              ; balance_change = Amount.Signed.(negate (of_unsigned amount))
              ; increment_nonce = double_sender_nonce
              ; events = []
              ; actions = []
              ; call_data = Snark_params.Tick.Field.zero
              ; call_depth = 0
              ; preconditions =
                  { Account_update.Preconditions.network =
                      { Zkapp_precondition.Protocol_state.accept with
                        global_slot_since_genesis = fst global_slot_precondition
                      }
                  ; account = Zkapp_precondition.Account.accept
                  ; valid_while = fst valid_while
                  }
              ; may_use_token = No
              ; use_full_commitment
              ; implicit_account_creation_fee = true
              ; authorization_kind =
                  ( if use_full_commitment then Signature
                  else Proof Zkapp_basic.F.zero )
              }
            ~authorization:
              ( if use_full_commitment then
                Control.Poly.Signature Signature.dummy
              else Proof (Lazy.force Mina_base.Proof.transaction_dummy) )
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
                  { Account_update.Preconditions.network =
                      { Zkapp_precondition.Protocol_state.accept with
                        global_slot_since_genesis = snd global_slot_precondition
                      }
                  ; account = Zkapp_precondition.Account.accept
                  ; valid_while = snd valid_while
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
  let zkapp_command =
    Zkapp_command.of_simple ~signature_kind ~proof_cache_db zkapp_command
  in
  let commitment = Zkapp_command.commitment zkapp_command in
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete commitment
      ~memo_hash:(Signed_command_memo.hash zkapp_command.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create ~signature_kind
           (Account_update.of_fee_payer zkapp_command.fee_payer) )
  in
  let account_updates_signature =
    let c = if use_full_commitment then full_commitment else commitment in
    Schnorr.Chunked.sign ~signature_kind sender.private_key
      (Random_oracle.Input.Chunked.field c)
  in
  let account_updates =
    Zkapp_command.Call_forest.map zkapp_command.account_updates
      ~f:(fun
           (account_update : (Account_update.Body.t, _, _) Account_update.Poly.t)
         ->
        match account_update.body.authorization_kind with
        | Signature ->
            { account_update with
              authorization = Control.Poly.Signature account_updates_signature
            }
        | _ ->
            account_update )
  in
  let signature =
    Schnorr.Chunked.sign ~signature_kind sender.private_key
      (Random_oracle.Input.Chunked.field full_commitment)
  in
  { zkapp_command with
    fee_payer = { zkapp_command.fee_payer with authorization = signature }
  ; account_updates
  }

module Handle = struct
  type valid

  type invalid

  type ('a, _) t = Valid : 'a -> ('a, valid) t | Invalid : (_, invalid) t

  type 'a valid_t = ('a, valid) t

  let make x = Valid x

  let invalidate (t : ('a, valid) t) : ('a, invalid) t =
    match t with Valid _ -> Invalid

  let get (t : ('a, valid) t) = match t with Valid x -> x

  module Operator = struct
    let ( ! ) = get
  end
end

module Sequencer_spec = struct
  type t =
    { outer_kp : Keypair.t
    ; holder_kp : Keypair.t
    ; token_holder_kp : Keypair.t
    ; signer_pk : Public_key.Compressed.t
    ; ephemeral_ledger : L.t (* The ledger to test the expected outcome *)
    ; specs : Transaction_spec.t list (* Transaction specs *)
    ; sequencer : Sequencer.t Handle.valid_t
    ; da_keys : Public_key.Compressed.t list
    ; accounts : Keypair.t list
    ; l1_config : Utils.Slot.l1_config
    ; l1_executor : Executor.t
    ; l2_executor : Executor.t
    }

  let gen ?(delay_deposit = 0) ?(number_of_transactions = 5) ?db_dir
      ?checkpoints_dir ?(commit_validity_period = Global_slot_span.of_int 10)
      ~logger ~postgres_uri ~gql_uri ~da_config ~da_keys ~da_quorum ~mq_host
      ~slot_acceptance () =
    let _reset =
      run @@ fun () -> Gql_client.For_tests.reset_state ~logger gql_uri
    in
    let deploy_config =
      Option.value_exn ~message:"ZEKO_DEPLOY_CONFIG is not set"
        Zeko_circuits_config.deploy_config
    in
    let outer_kp = Keypair.of_private_key_exn deploy_config.zeko_l1 in
    let holder_kp =
      Keypair.of_private_key_exn @@ List.hd_exn deploy_config.holder_accounts_l1
    in
    let token_holder_kp =
      Keypair.of_private_key_exn deploy_config.helper_token_owner_l1
    in
    print_endline "(* Connect signer *)" ;
    let signer_sk =
      Sys.getenv_exn "ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY"
      |> Private_key.of_base58_check_exn
    in
    let signer_keypair = Keypair.of_private_key_exn signer_sk in
    let signer =
      run (fun () ->
          let location =
            Sys.getenv_exn "ZEKO_TEST_SEQUENCER_SIGNER"
            |> Host_and_port.of_string
          in
          Signer_service.Client.create ~logger ~location
          >>| Signer_service.Signer.of_client )
    in
    let signer_pk = Signer_service.Signer.public_key signer in
    run (fun () ->
        let%bind _res =
          Gql_client.For_tests.create_account ~logger gql_uri signer_pk
        in
        return () ) ;

    let%bind.Quickcheck.Generator { init_ledger; specs } =
      Test_spec.mk_gen ~num_transactions:number_of_transactions ()
    in
    let funded_accounts =
      Array.init 10 ~f:(fun _ ->
          (Keypair.create (), Int64.of_float (1000. *. 1e8)) )
    in

    let `Inner inner_account, `Holder holder_account =
      run Deploy.Z.Inner.initial_accounts
    in
    let genesis_accounts =
      ( Account_id.create inner_account.public_key inner_account.token_id
      , inner_account )
      :: ( Account_id.create holder_account.public_key holder_account.token_id
         , holder_account )
      :: ( Array.concat [ init_ledger; funded_accounts ]
         |> Array.map ~f:(fun (keypair, balance) ->
                let pk = Signature_lib.Public_key.compress keypair.public_key in
                let account_id = Account_id.create pk Token_id.default in
                let balance = Unsigned.UInt64.of_int64 balance in
                let account =
                  Account.create account_id (Currency.Balance.of_uint64 balance)
                in
                (account_id, account) )
         |> Array.to_list )
    in

    print_endline "(* Init ephemeral ledger *)" ;
    let ephemeral_ledger =
      L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
    in
    List.iter genesis_accounts ~f:(fun (aid, acc) ->
        L.create_new_account_exn ephemeral_ledger aid acc ) ;
    let account_set_hash =
      let db =
        Indexed_merkle_tree.Db.create ~depth:constraint_constants.ledger_depth
          ()
      in
      let tids =
        List.map genesis_accounts ~f:(fun (aid, _) ->
            Account_id.derive_token_id ~owner:aid )
      in
      List.iter tids ~f:(fun tid ->
          let _, _ = Indexed_merkle_tree.Db.get_or_create_entry_exn db tid in
          () ) ;
      Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root db |]
    in

    print_endline "(* Post genesis batch *)" ;
    run (fun () ->
        Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
          ~ledger:ephemeral_ledger ) ;

    print_endline "(* Deploy zkapp *)" ;
    run (fun () ->
        let sequencer_pk = signer_pk |> Even_PC.create_exn in
        ( print_endline
        @@ Public_key.(
             Compressed.to_base58_check @@ compress outer_kp.public_key) ) ;
        ( print_endline
        @@ Public_key.(
             Compressed.to_base58_check @@ compress holder_kp.public_key) ) ;
        ( print_endline
        @@ Public_key.(
             Compressed.to_base58_check @@ compress token_holder_kp.public_key)
        ) ;
        let%bind nonce =
          Gql_client.infer_nonce ~logger gql_uri signer_pk >>| Or_error.ok_exn
        in
        let%bind command =
          let da_key =
            Multisig.commit
              { public_keys = da_keys; quorum = Field.of_int da_quorum }
          in
          printf "Deplying with DA key: %s\n%!" (Field.to_string da_key) ;
          Deploy.deploy_command_exn
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
            ~signer:signer_keypair ~outer_kp ~holder_kp ~token_holder_kp
            ~fee:(Currency.Fee.of_mina_int_exn 1)
            ~nonce ~initial_ledger:ephemeral_ledger
            ~account_creation_fee:constraint_constants.account_creation_fee
            ~account_set_hash ~pause_key:sequencer_pk ~sequencer:sequencer_pk
            ~da_key ()
        in
        let%bind _ =
          Gql_client.send_zkapp gql_uri
            (Zkapp_command.read_all_proofs_from_disk command)
        in
        let%bind _created =
          Gql_client.For_tests.create_new_block ~logger gql_uri
        in
        return () ) ;

    let l1_config : Utils.Slot.l1_config =
      let genesis_timestamp =
        run
        @@ fun () ->
        Gql_client.fetch_genesis_timestamp ~logger gql_uri >>| Or_error.ok_exn
      in
      { fork_timestamp = genesis_timestamp
      ; fork_slot = Mina_numbers.Global_slot_since_genesis.zero
      }
    in

    print_endline "(* Init sequencer *)" ;

    let sequencer =
      run (fun () ->
          Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
            ~da_config ~da_keys ~da_quorum ~db_dir ~postgres_uri ~l1_uri:gql_uri
            ~archive_uri:gql_uri ~signer ~deposit_delay_blocks:delay_deposit
            ~mq_host ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
            ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
            ~l1_config ~commit_validity_period ~checkpoints_dir )
    in
    (* The L2 executor below uses [funded_accounts.(0)] as its signer (since
       the sequencer's own signer has no balance on L2 in this test setup).
       Tell the sequencer to use the same key when preverifying L2 commands so
       preverify reflects what the executor will actually submit. *)
    Sequencer.set_l2_fee_payer_pk sequencer
      (Public_key.compress (fst funded_accounts.(0)).public_key) ;
    let l1_executor =
      Executor.create ~kind:(`L1 gql_uri)
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 ~signer ()
    in
    let l2_executor =
      Executor.create
        ~kind:
          (`L2
            { infer_nonce = Sequencer.infer_nonce sequencer
            ; apply_user_command = Sequencer.apply_user_command sequencer
            } )
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
        ~signer:(Signer_service.Signer.of_keypair (funded_accounts.(0) |> fst))
        ()
    in
    Quickcheck.Generator.return
      { outer_kp
      ; holder_kp
      ; token_holder_kp
      ; signer_pk
      ; ephemeral_ledger
      ; specs
      ; sequencer = Handle.make sequencer
      ; da_keys
      ; accounts = Array.map funded_accounts ~f:fst |> Array.to_list
      ; l1_config
      ; l1_executor
      ; l2_executor
      }
end
