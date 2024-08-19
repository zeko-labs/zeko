open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

open Zkapp_basic
open Account_update
module Nat = Pickles_types.Nat
module Nonce = Mina_numbers.Account_nonce
module L = Mina_ledger.Ledger
module Public_key = Signature_lib.Public_key
module PC = Public_key.Compressed
module CAS = Currency.Amount.Signed
module CA = Currency.Amount
open Zeko_util

(** The rules for the inner account zkapp, that controls the money supply and transfers on the rollup *)
module Inner = struct
  let public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x @@ Field.Constant.of_int 123456789)
    in
    Public_key.compress pk

  module State = struct
    type t = { outer_action_state : F.t  (** Action state of outer account. *) }
    [@@deriving snarky]

    let default : t = { outer_action_state = Actions.empty_state_element }

    let var_of_app_state (outer_action_state :: _ : F.var Zkapp_state.V.t) : var
        =
      { outer_action_state }

    let value_of_app_state (outer_action_state :: _ : F.t Zkapp_state.V.t) : t =
      { outer_action_state }
  end

  module Witness = struct
    type t = { vk_hash : F.t; ext : Action_state_extension.t }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ vk_hash; ext } = exists_witness () in
    let account_update = Body.(constant (typ ()) dummy) in
    let old_outer_action_state =
      (Action_state_extension.statement_var ext).source
    in
    let outer_action_state =
      (Action_state_extension.statement_var ext).target
    in
    let update =
      { account_update.update with
        app_state =
          State.(var_to_app_state typ { outer_action_state })
          (* This is equal to outer state action state and is checked in outer account rule *)
      }
    in
    let preconditions =
      { Preconditions.(constant (typ ()) accept) with
        account =
          { Zkapp_precondition.Account.(constant (typ ()) accept) with
            state = State.(var_to_precondition typ { outer_action_state = old_outer_action_state })
          }
      }
    in
    let account_update =
      { account_update with
        public_key = constant PC.typ public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update
      ; preconditions
      }
    in
    let public_output, auxiliary_output =
      make_outputs account_update (Zkapp_call_forest.Checked.empty ())
    in
    Pickles.Inductive_rule.
      { previous_proof_statements = [ Action_state_extension.verify ext ]
      ; public_output
      ; auxiliary_output
      }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup inner account step"
    ; prevs = [ force Action_state_extension.tag ]
    ; main
    ; feature_flags
    }
end

module G = struct
  include Mina_numbers.Global_slot_since_genesis

  type var = Checked.t
end

(** Rules for outer account zkapp *)
module Outer = struct
  module State = struct
    (* NB! Don't change this, code depends on the layout, sorry *)
    type t =
      { ledger_hash : Ledger_hash.t  (** The ledger hash of the rollup *)
      ; inner_action_state : F.t
      ; sequencer : PC.t
      ; macroslot : G.t
      ; bid_amount : CA.t
      ; finalized : Boolean.t
            (** All withdrawals registered on the L2. They are paid out on the L1. *)
      }
    [@@deriving snarky]

    (* FIXME: Define through HKD *)
    module Precondition = struct
      (* NB! Don't change this, code depends on the layout, sorry *)
      type t =
        { ledger_hash : Ledger_hash.var option
              (** The ledger hash of the rollup *)
        ; inner_action_state : F.var option
        ; sequencer : PC.var option
        ; macroslot : G.var option
        ; bid_amount : CA.var option
        ; finalized : Boolean.var option
              (** All withdrawals registered on the L2. They are paid out on the L1. *)
        }

      let to_precondition (t : t) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
        var_to_precondition_fine Var_to_precondition_fine.(
          [ (Ledger_hash.typ, t.ledger_hash)
          ; (F.typ, t.inner_action_state)
          ; (PC.typ, t.sequencer)
          ; (G.typ, t.macroslot)
          ; (CA.typ, t.bid_amount)
          ; (Boolean.typ, t.finalized)
          ]
        )
    end
  end

  module Slot_range = struct
    type t = { lower : G.t; upper : G.t } [@@deriving snarky]
  end

  module Action = struct
    module Commit = struct
      type t =
        { ledger : Ledger_hash.t
        ; inner_action_state : F.t
        ; sequencer : PC.t
        ; macroslot : G.t
        ; bid_amount : CA.t
        ; slot_range : Slot_range.t
        }
      [@@deriving snarky]
    end

    module Bid = struct
      type t =
        { sequencer : PC.t
        ; bid_amount : CA.t
        ; slot_range_bid : Slot_range.t
        ; slot_range : Slot_range.t
        }
      [@@deriving snarky]
    end

    module Zkapp_call_forest = struct
      include Zkapp_call_forest

      type var = Checked.t
    end

    module Witness = struct
      type t = { aux : F.t; children : Zkapp_call_forest.t } [@@deriving snarky]
    end

    type t =
      | Commit of Commit.t
      | Bid of Bid.t
      | Witness of Witness.t
      | Time of Slot_range.t

    module Repr = struct
      (* TODO: consider adding check to typ to ensure
         only one case is ever filled. *)
      type t =
        { tag : F.t
        ; case_commit : Commit.t
        ; case_bid : Bid.t
        ; case_witness : Witness.t
        ; case_time : Slot_range.t
        }
      [@@deriving snarky]
    end

    type var = Repr.var
    
    let tag_commit = Field.Constant.of_int 0
    let tag_bid = Field.Constant.of_int 1
    let tag_witness = Field.Constant.of_int 2
    let tag_time = Field.Constant.of_int 3
    
    let is_commit (x : var) = Field.(equal x.tag (constant tag_commit))
    let is_bid (x : var) = Field.(equal x.tag (constant tag_bid))
    let is_witness (x : var) = Field.(equal x.tag (constant tag_witness))
    let is_time (x : var) = Field.(equal x.tag (constant tag_time))

    let dummy_commit : Commit.t =
      { ledger = Outside_hash_image.t
      ; inner_action_state = Outside_hash_image.t
      ; sequencer = PC.empty
      ; macroslot = G.zero
      ; bid_amount = CA.zero
      ; slot_range = { lower = G.zero; upper = G.zero }
      }

    let dummy_bid : Bid.t =
      { sequencer = PC.empty
      ; bid_amount = CA.zero
      ; slot_range_bid = { lower = G.zero; upper = G.zero }
      ; slot_range = { lower = G.zero; upper = G.zero }
      }

    let dummy_witness : Witness.t =
      { aux = Field.Constant.zero; children = Zkapp_call_forest.empty () }

    let dummy_time : Slot_range.t = {
      lower = G.zero
      ; upper = G.zero
    }

    let there : t -> Repr.t = function
      | Commit c ->
          { tag = tag_commit
          ; case_commit = c
          ; case_bid = dummy_bid
          ; case_witness = dummy_witness
          ; case_time = dummy_time
          }
      | Bid b ->
          { tag = tag_bid
          ; case_commit = dummy_commit
          ; case_bid = b
          ; case_witness = dummy_witness
          ; case_time = dummy_time
          }
      | Witness w ->
          { tag = tag_witness
          ; case_commit = dummy_commit
          ; case_bid = dummy_bid
          ; case_witness = w
          ; case_time = dummy_time
          }
      | Time t ->
          { tag = tag_time
          ; case_commit = dummy_commit
          ; case_bid = dummy_bid
          ; case_witness = dummy_witness
          ; case_time = t
          }

    let back : Repr.t -> t =
     fun x ->
      if Field.Constant.(equal x.tag tag_commit) then Commit x.case_commit
      else if Field.Constant.(equal x.tag tag_bid) then Bid x.case_bid
      else if Field.Constant.(equal x.tag tag_witness) then
        Witness x.case_witness
      else if Field.Constant.(equal x.tag tag_time) then Time x.case_time
      else failwith "should be unreachable"

    let typ : (var, t) Typ.t = Typ.transport ~there ~back Repr.typ
  end

  module SwitchSequencer = struct
    let bid_step (cur_sequencer : PC.var) (cur_bid_amount : CA.var) (next_action : Action.var) =
      let is_better =
        Boolean.(
        Action.is_bid next_action &&
        run CA.Checked.(next_action.case_bid.bid_amount > cur_bid_amount)
        )
        in
      let bid_amount = CA.Checked.if_ is_better ~then_:next_action.case_bid.bid_amount ~else_:cur_bid_amount in
      let sequencer = PC.Checked.if_ is_better ~then_:next_action.case_bid.sequencer ~else_:cur_sequencer in
      (sequencer, bid_amount)
  end

  module Commit = struct
    module PathElt = struct
      type t = { right_side : F.t } [@@deriving snarky]
    end

    module Path =
      SnarkList
        (PathElt)
        (struct
          let length = constraint_constants.ledger_depth
        end)

    module Witness = struct
      type t =
        { txn_snark : Wrapper.t  (** The ledger transition we are performing. *)
        ; public_key : PC.t  (** Our public key on the L2 *)
        ; vk_hash : F.t  (** Our vk hash *)
        ; sequencer : PC.t  (** Sequencer public key *)
        ; bid_amount : CA.t
        ; macroslot : G.t  (** Current macroslot *)
        ; slot_range : Slot_range.t  (** slot_range *)
        ; action_state_extension : Action_state_extension.t
        ; old_inner_acc : Account.t
        ; old_inner_acc_path : Path.t
        ; new_inner_acc : Account.t
              (** Withdrawals to be processed this time *)
        ; new_inner_acc_path : Path.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    (* Copied from transaction_snark/transaction_snark.ml. There's
       similar code in snarky's merkle tree code, but I(Las)'m too lazy to check
       if it's correct for our purposes. *)
    let implied_root account (path : Path.var) : F.var =
      let open Impl in
      List.foldi path
        ~init:(run @@ Account.Checked.digest account)
        ~f:(fun height acc PathElt.{ right_side } ->
          Ledger_hash.merge_var ~height acc right_side )

    let get_zkapp (a : Account.var) : Zkapp_account.Checked.t =
      let hash, content = a.zkapp in
      let content =
        exists Zkapp_account.typ ~compute:(fun () ->
            match As_prover.Ref.get content with
            | Some content ->
                content
            | None ->
                Zkapp_account.default )
      in
      with_label __LOC__ (fun () ->
          Field.Assert.equal hash @@ Zkapp_account.Checked.digest content ) ;
      content

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let ({ txn_snark
           ; public_key
           ; vk_hash
           ; macroslot
           ; sequencer
           ; bid_amount
           ; slot_range
           ; action_state_extension
           ; old_inner_acc
           ; old_inner_acc_path
           ; new_inner_acc
           ; new_inner_acc_path
           }
            : Witness.var ) =
        exists_witness ()
      in
      let implied_root_old = implied_root old_inner_acc old_inner_acc_path in
      let implied_root_new = implied_root new_inner_acc new_inner_acc_path in

      (* We check that the paths provided for the inner account are correct. *)
      with_label __LOC__ (fun () ->
          Field.Assert.equal
            (Ledger_hash.var_to_hash_packed
               (Wrapper.statement_var txn_snark).source_ledger )
            implied_root_old ) ;
      with_label __LOC__ (fun () ->
          Field.Assert.equal
            (Ledger_hash.var_to_hash_packed
               (Wrapper.statement_var txn_snark).target_ledger )
            implied_root_new ) ;

      (* We check that we're dealing with the correct account. *)
      with_label __LOC__ (fun () ->
          run
          @@ PC.Checked.Assert.equal old_inner_acc.public_key
               (constant PC.typ Inner.public_key) ) ;
      (* We repeat the above check for the new account. *)
      with_label __LOC__ (fun () ->
          run
          @@ PC.Checked.Assert.equal new_inner_acc.public_key
               @@ constant PC.typ Inner.public_key ) ;

      let old_inner_zkapp = get_zkapp old_inner_acc in
      let new_inner_zkapp = get_zkapp new_inner_acc in

      let outer_action_state_in_inner =
        (Inner.State.var_of_app_state new_inner_zkapp.app_state)
          .outer_action_state
      in

      (* We want to transfer only deposits finalised with some certainty.
         By submitting `delay_extension` we can prove that we are transfering older deposits. *)
      with_label __LOC__ (fun () ->
          Field.Assert.equal
            (Action_state_extension.statement_var action_state_extension).source
            outer_action_state_in_inner ) ;

      (* Withdrawals are registered in the inner account's action state *)
      let (old_inner_action_state :: _) = old_inner_zkapp.action_state in
      let (inner_action_state :: _) = new_inner_zkapp.action_state in

      (* Init account update *)
      let account_update = Body.(constant (typ ()) dummy) in

      (* Finalize update  *)
      let update =
        { account_update.update with
          app_state =
            State.(
              var_to_app_state typ
                ( { ledger_hash = (Wrapper.statement_var txn_snark).target_ledger
                  ; inner_action_state
                  ; sequencer
                  ; macroslot
                  ; bid_amount
                  ; finalized = Boolean.false_
                  }
                  : var ))
        }
      in
      let preconditions =
        { Preconditions.(constant (typ ()) accept) with
          account =
            { Zkapp_precondition.Account.(constant (typ ()) accept) with
              state =
                State.Precondition.(
                  to_precondition
                    { ledger_hash =
                        Some (Wrapper.statement_var txn_snark).source_ledger
                    ; inner_action_state = Some old_inner_action_state
                    ; sequencer = Some sequencer
                    ; macroslot = Some macroslot
                    ; bid_amount = Some bid_amount
                    ; finalized = Some Boolean.false_
                    })
            ; action_state =
                Or_ignore.Checked.make_unsafe Boolean.true_
                  (Action_state_extension.statement_var action_state_extension)
                    .target
                (* Our action state must match *)
            }
        ; valid_while
        }
      in
      (* Our account update is assembled, specifying our state update, our preconditions, our pk, and our authorization *)
      let account_update =
        { account_update with
          public_key
        ; authorization_kind = authorization_vk_hash vk_hash
        ; update
        ; preconditions
        }
      in

      let sequencer_account_update =
        attach_control_var
          { Body.(constant (typ ()) dummy) with
            public_key = sequencer
          ; authorization_kind = authorization_signed ()
          ; use_full_commitment = Boolean.true_
          }
      in

      let calls =
        Zkapp_call_forest.Checked.(
          push ~account_update:sequencer_account_update ~calls:(empty ())
            (empty ()))
      in

      (* Assemble some stuff to help the prover and calculate public output *)
      let public_output, auxiliary_output = make_outputs account_update calls in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ Wrapper.verify txn_snark
              (* Proof for Wrapper showing there is a valid transition from source to target *)
            ; Action_state_extension.verify action_state_extension
              (* Proof that deposits as recorded on L1 went forward, otherwise it could go backwards,
                 and proof that the action_state precondition is an extension of our new all_deposits *)
            ]
        ; public_output
        ; auxiliary_output
        }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step"
      ; prevs = [ tag; force Action_state_extension.tag ]
      ; main
      ; feature_flags
      }
  end
end

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
