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

(**
  This is the state for the helper accounts.
  Each user has a helper account they create for handling transfers.
  Keep in mind that the transfers recorded is the action state, i.e.
  a merkle list of transfers.
  `transfers_processed` is like a pointer into that list.
  It is literally the root of the merkle list at some point in time,
  and is ~guaranteed to be unique.
  The user processes their transfer by reducing the balance from the main
  account and updating the pointer to the point in the merkle list
  after the transfer we processed.
*)
module Helper_state = struct
  type t = { transfers_processed : F.t } [@@deriving snarky]

  let value_of_app_state (transfers_processed :: _ : F.t Zkapp_state.V.t) : t =
    { transfers_processed }

  (* FIXME: Define through HKD *)
  module Precondition = struct
    type t = { transfers_processed : F.var option }

    let to_precondition (t : t) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
      [ ( match t.transfers_processed with
        | Some x ->
            Or_ignore.Checked.make_unsafe Boolean.true_ x
        | None ->
            ignore )
      ; ignore
      ; ignore
      ; ignore
      ; ignore
      ; ignore
      ; ignore
      ; ignore
      ]
  end
end

(** The type for "transfer requests" put in actions *)
module TR = struct
  type t =
    { amount : CA.t  (** Amount to be transferred *)
    ; recipient : PC.t  (** Recipient, i.e. pk of helper account *)
    }
  [@@deriving snarky]

  let to_actions (t : t) : Actions.t = value_to_actions typ t
end

(**
  Rule used by both inner and outer accounts to process transfers posted to them.
  
  The rule takes as witness a proof that the transfer has been submitted,
  and that it hasn't been processed by this account already.
  The recipient has an associated helper account which tracks what transfers
  this recipient has had processed.
  There must be an action state extension from the old root to the point just
  before the transfer to be processed, and one from the root at the time of the
  transfer until the current all_transfers, i.e. the root of all transfers in existence.
  
  If the recipient is new a trivial optimization is done since it's impossible
  for a transfer to already have been processed.
  
  The actual payout is done by including a child to which the transfer is sent.
  
  There is notably a seemingly unnecessary overhead here.
  Why do we need to verify which account receives the funds?
  Couldn't we abstain from enforcing its use entirely?
  Then the whole transaction could be signed by the recipient,
  and when signing they could verify that the funds go where they want
  (which might be somewhere else entirely!).
  
  We can't however check the signature on the whole transaction!
  The only thing we can do is have a child account update with use_full_commitment = true,
  and a signature on that, and we've done that here.
  It also has the benefit of working better with smart contracts.
  
  Optimally Mina would expose the full commitment as an input to smart contracts too.
  
  NB: Helper account is both made and updated in this rule.
*)
module Process_transfer = struct
  module A = struct
    type t = Zkapp_call_forest.account_update

    type var = Zkapp_call_forest.Checked.account_update

    let typ = Zkapp_call_forest.Checked.account_update_typ ()
  end

  module Witness = struct
    type t =
      { transfer : TR.t  (** The transfer *)
      ; child : A.t
      ; vk_hash : F.t
      ; public_key : PC.t
      ; trans1 : Action_state_extension.t
            (** From transfers_processed to transfer we are to process *)
      ; trans2 : Action_state_extension.t
            (** From transfer we are to process + 1 to all_transfers *)
      ; is_new : Boolean.t
      }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  type to_precondition =
    all_transfers:F.var option -> F.var Or_ignore.Checked.t Zkapp_state.V.t

  let%snarkydef_ main (to_precondition : to_precondition)
      Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ transfer; child; vk_hash; public_key; trans1; trans2; is_new }
        =
      exists_witness ()
    in
    let child' = child.account_update.data in

    (* The recipient must match *)
    with_label __LOC__ (fun () ->
        run @@ PC.Checked.Assert.equal child'.public_key transfer.recipient ) ;
    (* It must be a recipient using the default token (MINA), otherwise it's the wrong account *)
    with_label __LOC__ (fun () ->
        Token_id.(Checked.Assert.equal child'.token_id (constant typ default)) ) ;
    (* The amount must match *)
    with_label __LOC__ (fun () ->
        run
        @@ CAS.Checked.(
             assert_equal child'.balance_change (of_unsigned transfer.amount)) ) ;

    (* Check the connection between trans1 and trans2 *)
    with_label __LOC__ (fun () ->
        Field.Assert.equal (Action_state_extension.statement_var trans2).source
        @@ Actions.push_events_checked
             (Action_state_extension.statement_var trans1).target
             (var_to_actions TR.typ transfer) ) ;

    let transfers_processed =
      (Action_state_extension.statement_var trans1).source
    in
    (* If it's a new account,
       then trans1 must begin at the beginning.
       We however don't use the value, so technically this is superfluous,
       but future changes to this code might not take that into account,
       hence we add this check just in case, to make sure it's not set to
       an invalid value.
    *)
    assert_var __LOC__ (fun () ->
        Boolean.(
          Field.(
            equal transfers_processed (constant Actions.empty_state_element))
          ||| not is_new) ) ;
    (* Helper account account update *)
    let child_helper = Body.(constant (typ ()) dummy) in
    let child_helper =
      { child_helper with
        public_key = transfer.recipient
      ; token_id = public_key_to_token_id_var public_key
      ; use_full_commitment = Boolean.true_
      ; authorization_kind =
          authorization_signed () (* FIXME: permit proof auth *)
      ; preconditions =
          { Preconditions.(constant (typ ()) accept) with
            account =
              { Zkapp_precondition.Account.(constant (typ ()) accept) with
                state =
                  Helper_state.Precondition.(
                    to_precondition
                      { transfers_processed =
                          Some
                            Field.(
                              if_ is_new ~then_:zero ~else_:transfers_processed)
                          (* This precondition is to ensure that transfers are only processed once.
                             For a new account, the field is zero. *)
                      })
                  (* If the account is new, we know that no transfers can have been processed,
                     logically letting the user "set" the field beforehand to an arbitrary value. *)
              ; is_new = Or_ignore.Checked.make_unsafe Boolean.true_ is_new
              }
          }
      ; update =
          { child_helper.update with
            app_state =
              Helper_state.(
                var_to_app_state typ
                  { transfers_processed =
                      (Action_state_extension.statement_var trans2).source
                  })
          }
      ; may_use_token =
          May_use_token.Checked.constant Parents_own_token
          (* We give it permission *)
      ; implicit_account_creation_fee =
          Boolean.false_ (* Custom token account, can't be true *)
      }
    in
    let child_helper : Zkapp_call_forest.Checked.account_update =
      attach_control_var child_helper
    in

    (* FIXME: Recipient can't have children *)
    let calls =
      Zkapp_call_forest.Checked.(
        push ~account_update:child ~calls:(empty ())
          (push ~account_update:child_helper ~calls:(empty ()) @@ empty ()))
    in
    (* We check that the transfer has been submitted *)
    let preconditions =
      { Preconditions.(constant (typ ()) accept) with
        account =
          { Zkapp_precondition.Account.(constant (typ ()) accept) with
            state =
              to_precondition
                ~all_transfers:
                  (Some (Action_state_extension.statement_var trans2).target)
          }
      }
    in
    let account_update =
      { Body.(constant (typ ()) dummy) with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; preconditions
      ; balance_change =
          CAS.Checked.(negate @@ of_unsigned transfer.amount)
          (* We pay out the amount *)
      }
    in
    let public_output, auxiliary_output = make_outputs account_update calls in
    Pickles.Inductive_rule.
      { previous_proof_statements =
          [ Action_state_extension.verify ~check:(Boolean.not is_new) trans1
            (* If the account is new, then we needn't check it,
               since source must be the empty element,
               and we know the relation holds since the end is connected to a real action state,
               which of course comes from the empty element. *)
          ; Action_state_extension.verify trans2
          ]
      ; public_output
      ; auxiliary_output
      }

  let rule (to_precondition : to_precondition) action_state_extension_tag :
      _ Pickles.Inductive_rule.t =
    { identifier = "zeko process transfer"
    ; prevs = [ action_state_extension_tag; action_state_extension_tag ]
    ; main = main to_precondition
    ; feature_flags
    }
end

(** Rule used by both inner and outer accounts to validate actions posted to account *)
module Submit_transfer = struct
  module Witness = struct
    type t =
      { public_key : PC.t; vk_hash : F.t; amount : CA.t; recipient : PC.t }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ public_key; vk_hash; amount; recipient } =
      exists_witness ()
    in
    let account_update = Body.(constant (typ ()) dummy) in
    (* The amount will go the recipient on the other side *)
    let tr : TR.var = { amount; recipient } in
    (* We add a single action which is the above amount and recipient *)
    let empty_actions = Zkapp_account.Actions.(constant typ []) in
    let actions =
      Actions.push_to_data_as_hash empty_actions (var_to_fields TR.typ tr)
    in
    let account_update =
      { account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; balance_change = CAS.Checked.of_unsigned @@ amount
      ; actions
      }
    in
    let public_output, auxiliary_output =
      make_outputs account_update @@ Zkapp_call_forest.Checked.empty ()
    in
    Pickles.Inductive_rule.
      { previous_proof_statements = []; public_output; auxiliary_output }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "zeko submit transfer"; prevs = []; main; feature_flags }
end

(** The rules for the inner account zkapp, that controls the money supply and transfers on the rollup *)
module Inner = struct
  let public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x @@ Field.Constant.of_int 123456789)
    in
    Public_key.compress pk

  module State = struct
    type t =
      { all_deposits : F.t
            (** All deposits that have been made on the L1. They are paid out on the L2. *)
      }
    [@@deriving snarky]

    let default : t = { all_deposits = Actions.empty_state_element }

    (* FIXME: add prover-time check to ensure all other fields are zero for safety *)
    let var_of_app_state (all_deposits :: _ : F.var Zkapp_state.V.t) : var =
      { all_deposits }

    let value_of_app_state (all_deposits :: _ : F.t Zkapp_state.V.t) : t =
      { all_deposits }

    (* FIXME: Define through HKD *)
    module Precondition = struct
      type t = { all_deposits : F.var option }

      let to_precondition (t : t) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
        [ ( match t.all_deposits with
          | Some x ->
              Or_ignore.Checked.make_unsafe Boolean.true_ x
          | None ->
              ignore )
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ]
    end
  end

  module Witness = struct
    type t =
      { vk_hash : F.t
      ; all_deposits : F.t
            (* We update all_deposits to what the outer account tells us *)
            (* This _can't_ go wrong; if an incorrect one is chosen, the account will
               have an incorrect value in the app state, and the outer step will fail.
               Do note that outer step also checks the nonce. *)
      }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ vk_hash; all_deposits } = exists_witness () in
    let account_update = Body.(constant (typ ()) dummy) in
    let update =
      { account_update.update with
        app_state =
          State.(var_to_app_state typ { all_deposits })
          (* This is equal to outer state action state and is checked in outer account rule *)
      }
    in
    let account_update =
      { account_update with
        public_key = constant PC.typ public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update
      ; increment_nonce =
          Boolean.true_
          (* We increment the nonce to signal that a step has been performed to the outer account *)
      }
    in
    let public_output, auxiliary_output =
      make_outputs account_update (Zkapp_call_forest.Checked.empty ())
    in
    Pickles.Inductive_rule.
      { previous_proof_statements = []; public_output; auxiliary_output }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup inner account step"
    ; prevs = []
    ; main
    ; feature_flags
    }
end

(** Rules for outer account zkapp *)
module Outer = struct
  module State = struct
    (* NB! Don't change this, code depends on the layout, sorry *)
    type t =
      { ledger_hash : Ledger_hash.t  (** The ledger hash of the rollup *)
      ; all_withdrawals : F.t
            (** All withdrawals registered on the L2. They are paid out on the L1. *)
      }
    [@@deriving snarky]

    let value_of_app_state
        (ledger_hash :: all_withdrawals :: _ : F.t Zkapp_state.V.t) : t =
      { ledger_hash; all_withdrawals }

    (* FIXME: Define through HKD *)
    module Precondition = struct
      type t =
        { ledger_hash : Ledger_hash.var option; all_withdrawals : F.var option }

      let to_precondition (t : t) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
        [ ( match t.ledger_hash with
          | Some x ->
              Or_ignore.Checked.make_unsafe Boolean.true_
                (Ledger_hash.var_to_field x)
          | None ->
              ignore )
        ; ( match t.all_withdrawals with
          | Some x ->
              Or_ignore.Checked.make_unsafe Boolean.true_ x
          | None ->
              ignore )
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ; ignore
        ]
    end
  end

  module PathElt = struct
    type t = { is_right : Boolean.t; other : F.t } [@@deriving snarky]
  end

  module Path' = struct
    module T = PathElt

    let length = constraint_constants.ledger_depth
  end

  module Path = SnarkList (Path')

  (* Pickles allows to verify max 2 proofs recursively, so we wrap the extensions into separate proof *)
  module Extensions_wrapper = struct
    module Stmt = struct
      type t =
        { all_deposits : Action_state_extension.Stmt.t
        ; delay_extension : Action_state_extension.Stmt.t
        }
      [@@deriving snarky]
    end

    module Witness = struct
      type t =
        { all_deposits : Action_state_extension.t  (** Action state on the L1 *)
        ; delay_extension : Action_state_extension.t  (** To delay deposits *)
        }
      [@@deriving snarky]
    end

    type t = { stmt : Stmt.t; proof : RefProof.t } [@@deriving snarky]

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let ({ all_deposits; delay_extension } : Witness.var) =
        exists_witness ()
      in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ Action_state_extension.verify all_deposits
            ; Action_state_extension.verify delay_extension
            ]
        ; public_output =
            Stmt.
              { all_deposits = Action_state_extension.statement_var all_deposits
              ; delay_extension =
                  Action_state_extension.statement_var delay_extension
              }
        ; auxiliary_output = ()
        }

    let rule action_extension_tag : _ Pickles.Inductive_rule.t =
      { identifier = "wrapper for Outer.step extensions"
      ; prevs = [ action_extension_tag; action_extension_tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }

    let verify ({ stmt; proof } : var) :
        (Stmt.var, Nat.N2.n) Pickles.Inductive_rule.Previous_proof_statement.t =
      { public_input = stmt; proof_must_verify = Boolean.(true_); proof }
  end

  module Witness = struct
    type t =
      { t : Wrapper.t  (** The ledger transition we are performing. *)
      ; public_key : PC.t  (** Our public key on the L2 *)
      ; vk_hash : F.t  (** Our vk hash *)
      ; actions_extensions : Extensions_wrapper.t
      ; old_inner_acc : Account.t
      ; new_inner_acc : Account.t  (** Withdrawals to be processed this time *)
      ; old_inner_acc_path : Path.t  (** Old path to inner account *)
      ; new_inner_acc_path : Path.t  (** New path to inner account *)
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
      ~f:(fun height acc PathElt.{ is_right; other } ->
        let l = Field.if_ is_right ~then_:other ~else_:acc
        and r = Field.if_ is_right ~then_:acc ~else_:other in
        let acc' = Ledger_hash.merge_var ~height l r in
        acc' )

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
    let ({ t
         ; public_key
         ; vk_hash
         ; actions_extensions
         ; old_inner_acc_path
         ; old_inner_acc
         ; new_inner_acc_path
         ; new_inner_acc
         }
          : Witness.var ) =
      exists_witness ()
    in
    let ({ all_deposits; delay_extension } : Extensions_wrapper.Stmt.var) =
      actions_extensions.stmt
    in
    let implied_root_old = implied_root old_inner_acc old_inner_acc_path in
    let implied_root_new = implied_root new_inner_acc new_inner_acc_path in
    (* We check that the paths provided for the inner account are correct. *)
    with_label __LOC__ (fun () ->
        Field.Assert.equal
          (Ledger_hash.var_to_hash_packed
             (Wrapper.statement_var t).source_ledger )
          implied_root_old ) ;
    with_label __LOC__ (fun () ->
        Field.Assert.equal
          (Ledger_hash.var_to_hash_packed
             (Wrapper.statement_var t).target_ledger )
          implied_root_new ) ;
    (* FIXME: allow more than one inner update (#102) *)
    (* There must have been exactly one or zero "steps" in the inner account, checked by checking nonce *)
    assert_var __LOC__ (fun () ->
        Boolean.(
          run
          @@ Nonce.Checked.equal
               (run @@ Nonce.Checked.succ old_inner_acc.nonce)
               new_inner_acc.nonce
          ||| run @@ Nonce.Checked.equal old_inner_acc.nonce new_inner_acc.nonce) ) ;
    (* We check that we're dealing with the correct account. *)
    with_label __LOC__ (fun () ->
        run
        @@ PC.Checked.Assert.equal old_inner_acc.public_key
             (constant PC.typ Inner.public_key) ) ;
    (* We repeat the above check for the new account. *)
    with_label __LOC__ (fun () ->
        run
        @@ PC.Checked.Assert.equal new_inner_acc.public_key
             (constant PC.typ Inner.public_key) ) ;
    let old_inner_zkapp = get_zkapp old_inner_acc in
    let new_inner_zkapp = get_zkapp new_inner_acc in
    (* The inner account keeps track of _our_ action state, that's how it knows
       what transfer requests have been logged on the L1 to the L2.
       We check that it's been set to our current app state,
       and seemingly redundantly, that it's also an extension of the old one.
       It's in fact possible to go _backwards_ if we're not careful, since
       what we think is our action state is in fact just a recent one,
       so you could set the action state precondition to an old state and go backwards.
       I haven't considered whether it's problematic to go backwards, but we disallow it anyway. *)
    let ({ all_deposits = old_all_deposits } : Inner.State.var) =
      Inner.State.var_of_app_state old_inner_zkapp.app_state
    in
    let ({ all_deposits = new_all_deposits } : Inner.State.var) =
      Inner.State.var_of_app_state new_inner_zkapp.app_state
    in
    (* Check that it matches all_deposits witness *)
    with_label __LOC__ (fun () ->
        Field.Assert.equal all_deposits.source old_all_deposits ) ;
    with_label __LOC__ (fun () ->
        Field.Assert.equal all_deposits.target new_all_deposits ) ;
    (* We want to transfer only deposits finalised with some certainty.
       By submitting `delay_extension` we can prove that we are transfering older deposits. *)
    with_label __LOC__ (fun () ->
        Field.Assert.equal delay_extension.source new_all_deposits ) ;
    (* Init account update *)
    let account_update = Body.(constant (typ ()) dummy) in
    (* Withdrawals are registered in the inner account's action state *)
    let (new_all_withdrawals :: _) = new_inner_zkapp.action_state in
    (* Finalize update  *)
    let update =
      { account_update.update with
        app_state =
          State.(
            var_to_app_state typ
              ( { ledger_hash = (Wrapper.statement_var t).target_ledger
                ; all_withdrawals = new_all_withdrawals
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
                  { ledger_hash = Some (Wrapper.statement_var t).source_ledger
                  ; all_withdrawals = None
                  })
          ; action_state =
              Or_ignore.Checked.make_unsafe Boolean.true_ delay_extension.target
              (* Our action state must match *)
          }
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
    (* Assemble some stuff to help the prover and calculate public output *)
    let public_output, auxiliary_output =
      make_outputs account_update (Zkapp_call_forest.Checked.empty ())
    in
    Pickles.Inductive_rule.
      { previous_proof_statements =
          [ Wrapper.verify t
            (* Proof for Wrapper showing there is a valid transition from source to target *)
          ; Extensions_wrapper.verify actions_extensions
            (* Proof that deposits as recorded on L1 went forward, otherwise it could go backwards,
               and proof that the action_state precondition is an extension of our new all_deposits *)
          ]
      ; public_output
      ; auxiliary_output
      }

  let rule tag extensions_wrapper_tag : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup step"
    ; prevs = [ tag; extensions_wrapper_tag ]
    ; main
    ; feature_flags
    }
end

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

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

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

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

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

module type S = Zkapps_rollup_intf.S with type t := t and module TR := TR
