open Core_kernel

(*
(* What we expose from this module *)
type t = Wrapper.t [@@deriving yojson]

let source_ledger (t : t) = (Wrapper.statement t).source_ledger

let target_ledger (t : t) = (Wrapper.statement t).target_ledger

let inner_pending_coinbase_init = Wrapper.dummy_pc_init

let inner_pending_coinbase = Wrapper.dummy_pc

let inner_state_body = Wrapper.dummy_state_body

let inner_public_key = Inner.public_key

let inner_account_id =
  Account_id.of_public_key @@ Public_key.decompress_exn Inner.public_key

let read_inner_state (a : Account.t) =
  let zkapp = Option.value_exn a.zkapp in
  let ({ all_deposits } : Inner.State.t) =
    Inner.State.value_of_app_state zkapp.app_state
  in
  `All_deposits all_deposits

let read_outer_state (a : Account.t) =
  let zkapp = Option.value_exn a.zkapp in
  let ({ ledger_hash; all_withdrawals } : Outer.State.t) =
    Outer.State.value_of_app_state zkapp.app_state
  in
  (`Ledger_hash ledger_hash, `All_withdrawals all_withdrawals)

let read_token_account_state (a : Account.t) =
  let zkapp = Option.value_exn a.zkapp in
  let ({ transfers_processed } : Helper_state.t) =
    Helper_state.value_of_app_state zkapp.app_state
  in
  `Transfers_processed transfers_processed

(** Compile the circuits *)
module Make (T' : Transaction_snark.S) = struct
  open Async_kernel
  module Wrapper = Wrapper.Make (T')

  type transfer_prover =
    ( Action_state_extension.Stmt.t * (Action_state_extension.Stmt.t * unit)
    , Nat.N2.n * (Nat.N2.n * unit)
    , Nat.N2.n * (Nat.N2.n * unit)
    , unit
    , ( field Zkapp_statement.Poly.t
      * ( Body.t
        * Zkapp_command.Digest.Account_update.t
        * ( T.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.t )
      * (Nat.N2.n, Nat.N2.n) Pickles.Proof.t )
      Deferred.t )
    Pickles.Prover.t

  let process_transfer ~is_new ~pointer ~before ~after ~transfer ~vk_hash
      ~public_key (prover : transfer_prover) =
    let before = List.map ~f:(value_to_actions TR.typ) before in
    let after = List.map ~f:(value_to_actions TR.typ) after in
    let%bind trans1 =
      Action_state_extension.prove ~dummy:is_new ~source:pointer before
    in
    let pointer' =
      Actions.push_events (Action_state_extension.statement trans1).target
        (value_to_actions TR.typ transfer)
    in
    let%bind trans2 = Action_state_extension.prove ~source:pointer' after in
    let child =
      { Body.dummy with
        public_key = transfer.recipient
      ; balance_change = CAS.of_unsigned transfer.amount
      ; use_full_commitment = true
      ; authorization_kind = Signature
      }
    in
    let child = attach_control child in
    let%map _, tree, proof =
      prover
        ~handler:
          (Process_transfer.handler
             { vk_hash; public_key; transfer; child; trans1; trans2; is_new } )
        ()
    in
    (* If the account is new, we must pay out the account creation fee for the helper account.
       We accomplish that by paying the fee _after_ receiving our funds.
       After all, there may not be enough fees before then.
       The account holding the MINA received pays its own fees using implicit_account_creation_fee. *)
    let account_creation_fee = constraint_constants.account_creation_fee in
    let account_creation_fee_payer : Account_update.t =
      { body =
          { Body.dummy with
            public_key = transfer.recipient
          ; balance_change =
              CA.of_fee account_creation_fee |> CAS.of_unsigned |> CAS.negate
          ; use_full_commitment = true
          ; authorization_kind = Signature
          }
      ; authorization = None_given
      }
    in
    ( `Pointer pointer'
    , Zkapp_command.Call_forest.(
        cons_tree (mktree tree proof)
          ( if is_new then
            accumulate_hashes'
            @@ of_account_updates
                 ~account_update_depth:(fun _ -> 0)
                 [ account_creation_fee_payer ]
          else [] )) )

  module Inner = struct
    include Inner

    let to_precondition : Process_transfer.to_precondition =
     fun ~all_transfers ->
      State.Precondition.(to_precondition { all_deposits = all_transfers })

    let ( tag
        , _
        , _
        , Pickles.Provers.[ step_; submit_withdrawal_; process_deposit_ ] ) =
      time "Inner.compile" (fun () ->
          Pickles.compile () ~cache:Cache_dir.cache
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~public_input:(Output Zkapp_statement.typ)
            ~auxiliary_typ:(Prover_value.typ ())
            ~branches:(module Nat.N3)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"rollup inner account"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self:_ ->
              [ Inner.rule
              ; Submit_transfer.rule
              ; Process_transfer.rule to_precondition
                  (force Action_state_extension.tag)
              ] ) )

    let vk =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Pickles.Side_loaded.Verification_key.of_compiled tag )

    let vk_hash = Zkapp_account.digest_vk vk

    let submit_withdrawal ~withdrawal:({ amount; recipient } : TR.t) =
      time_async "Inner.submit_withdrawal" (fun () ->
          let%map _, tree, proof =
            submit_withdrawal_
              ~handler:
                (Submit_transfer.handler
                   { vk_hash; public_key; amount; recipient } )
              ()
          in
          mktree tree proof )

    let process_deposit ~is_new ~pointer ~before ~after ~deposit =
      time_async "Inner.process_deposit" (fun () ->
          process_transfer ~is_new ~pointer ~before ~after ~transfer:deposit
            ~vk_hash ~public_key process_deposit_ )

    let step ~all_deposits :
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.Tree.t
        Deferred.t =
      time_async "Inner.step" (fun () ->
          let%map _, tree, proof =
            let w : Inner.Witness.t = { vk_hash; all_deposits } in
            step_ ~handler:(Inner.handler w) ()
          in
          mktree tree proof )

    let account_id = inner_account_id

    let initial_account =
      { Account.empty with
        public_key
      ; balance = Currency.Balance.max_int
      ; permissions = proof_permissions
      ; zkapp =
          Some
            { Zkapp_account.default with
              app_state = Inner.State.(value_to_init_state typ default)
            ; verification_key =
                Some (Verification_key_wire.Stable.Latest.M.of_binable vk)
            }
      }
  end

  module Outer = struct
    include Outer

    module Extensions_wrapper = struct
      include Extensions_wrapper

      let tag, _, _, Pickles.Provers.[ prove_ ] =
        time "Extensions_wrapper.compile" (fun () ->
            Pickles.compile ()
              ~override_wrap_domain:Pickles_base.Proofs_verified.N1
              ~cache:Cache_dir.cache ~public_input:(Output Stmt.typ)
              ~auxiliary_typ:Typ.unit
              ~branches:(module Nat.N1)
              ~max_proofs_verified:(module Nat.N2)
              ~name:"wrapper for Outer.step extensions"
              ~constraint_constants:
                (Genesis_constants.Constraint_constants.to_snark_keys_header
                   constraint_constants )
              ~choices:(fun ~self:_ ->
                [ rule (force Action_state_extension.tag) ] ) )

      let prove all_deposits delay_extension =
        time_async "Extensions_wrapper.prove" (fun () ->
            let%map stmt, _, proof =
              prove_ ~handler:(handler { all_deposits; delay_extension }) ()
            in
            ({ stmt; proof } : t) )
    end

    let to_precondition : Process_transfer.to_precondition =
     fun ~all_transfers ->
      State.Precondition.(
        to_precondition { all_withdrawals = all_transfers; ledger_hash = None })

    let ( tag
        , _
        , _
        , Pickles.Provers.[ step_; submit_deposit_; process_withdrawal_ ] ) =
      time "Outer.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Output Zkapp_statement.typ)
            ~auxiliary_typ:(Prover_value.typ ())
            ~branches:(module Nat.N3)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"rollup"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self:_ ->
              [ rule Wrapper.tag Extensions_wrapper.tag
              ; Submit_transfer.rule
              ; Process_transfer.rule to_precondition
                  (force Action_state_extension.tag)
              ] ) )

    let vk =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Pickles.Side_loaded.Verification_key.of_compiled tag )

    let vk_hash = Zkapp_account.digest_vk vk

    let vk : Verification_key_wire.t = { data = vk; hash = vk_hash }

    let submit_deposit ~outer_public_key ~deposit:({ amount; recipient } : TR.t)
        =
      time_async "Outer.submit_deposit" (fun () ->
          let%map _, tree, proof =
            submit_deposit_
              ~handler:
                (Submit_transfer.handler
                   { vk_hash; public_key = outer_public_key; amount; recipient } )
              ()
          in
          mktree tree proof )

    let process_withdrawal ~is_new ~outer_public_key ~pointer ~before ~after
        ~withdrawal =
      time_async "Inner.process_withdrawal" (fun () ->
          process_transfer ~is_new ~public_key:outer_public_key ~pointer ~before
            ~after ~vk_hash ~transfer:withdrawal process_withdrawal_ )

    let step (t : t) ~(outer_public_key : PC.t) ~(new_deposits : TR.t list)
        ~(unprocessed_deposits : TR.t list)
        ~(old_inner_ledger : Mina_ledger.Sparse_ledger.t)
        ~(new_inner_ledger : Mina_ledger.Sparse_ledger.t) :
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.Tree.t
        Deferred.t =
      let old_idx =
        Mina_ledger.Sparse_ledger.find_index_exn old_inner_ledger
          Inner.account_id
      in
      let old_inner_acc =
        Mina_ledger.Sparse_ledger.get_exn old_inner_ledger old_idx
      in
      let old_inner_acc_path =
        List.map ~f:(function
          | `Left other ->
              ({ is_right = false; other } : PathElt.t)
          | `Right other ->
              ({ is_right = true; other } : PathElt.t) )
        @@ Mina_ledger.Sparse_ledger.path_exn old_inner_ledger old_idx
      in
      let new_idx =
        Mina_ledger.Sparse_ledger.find_index_exn new_inner_ledger
          Inner.account_id
      in
      let new_inner_acc =
        Mina_ledger.Sparse_ledger.get_exn new_inner_ledger new_idx
      in
      let new_inner_acc_path =
        List.map ~f:(function
          | `Left other ->
              ({ is_right = false; other } : PathElt.t)
          | `Right other ->
              ({ is_right = true; other } : PathElt.t) )
        @@ Mina_ledger.Sparse_ledger.path_exn new_inner_ledger new_idx
      in
      let ({ all_deposits = old_all_deposits } : Inner.State.t) =
        Inner.State.value_of_app_state
          (Option.value_exn old_inner_acc.zkapp).app_state
      in
      let ({ all_deposits = new_all_deposits } : Inner.State.t) =
        Inner.State.value_of_app_state
          (Option.value_exn new_inner_acc.zkapp).app_state
      in
      let%bind all_deposits =
        Action_state_extension.prove ~source:old_all_deposits
          (List.map ~f:(value_to_actions TR.typ) new_deposits)
      in
      assert (
        Field.Constant.equal
          (Action_state_extension.statement all_deposits).target
          new_all_deposits ) ;
      let%bind delay_extension =
        Action_state_extension.prove ~dummy:true ~source:new_all_deposits
          (List.map ~f:(value_to_actions TR.typ) unprocessed_deposits)
      in
      let%bind actions_extensions =
        Extensions_wrapper.prove all_deposits delay_extension
      in
      time_async "Outer.step" (fun () ->
          let%map _, tree, proof =
            let w : Witness.t =
              { vk_hash
              ; t
              ; actions_extensions
              ; public_key = outer_public_key
              ; old_inner_acc
              ; old_inner_acc_path
              ; new_inner_acc
              ; new_inner_acc_path
              }
            in
            step_ ~handler:(handler w) ()
          in
          mktree tree proof )

    let unsafe_deploy (ledger_hash : Ledger_hash.t) =
      let update =
        { Update.dummy with
          app_state =
            State.(
              value_to_app_state typ
                ( { ledger_hash; all_withdrawals = Actions.empty_state_element }
                  : t ))
        ; verification_key = Set vk
        ; permissions = Set proof_permissions
        }
      in
      update

    let deploy_exn (l : L.t) =
      if not (PC.equal Inner.public_key (L.get_at_index_exn l 0).public_key)
      then failwith "zeko outer deploy: ledger invalid"
      else () ;
      unsafe_deploy (L.merkle_root l)
  end
end
  *)
