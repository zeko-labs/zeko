open Core_kernel
open Async_kernel
open Mina_base
open Signature_lib
open Snark_params.Tick

module type S = sig
  type t

  module TR : sig
    type t
  end

  module Wrapper : sig
    (** Wrap a ledger transition.

       The wrapped transition must be "whole", and must be made using
       inner_pending_coinbase and inner_pending_coinbase_init,
       such that the pc target and source are equal.
       The connecting ledger must be the ledger between the two passes,
       i.e., the end of the first pass, and the beginning of the second pass,
       and as such, the passes must be connected.
       You can not wrap a single zkapp command segment either,
       you can only wrap at least a whole zkapp command.
    *)
    val wrap : Transaction_snark.t -> t Deferred.t

    (** Merge two wrapped ledger transitions, they must connect or this will fail. *)
    val merge : t -> t -> t Deferred.t

    (* FIXME: remove, unneeded *)
    val verify : t -> unit Or_error.t Deferred.t
  end

  module Inner : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    (** Account update for withdrawing *)
    val withdraw :
         public_key:Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> recipient:Public_key.Compressed.t
      -> ( Account_update.t
         , Zkapp_command.Digest.Account_update.t
         , Zkapp_command.Digest.Forest.t )
         Zkapp_command.Call_forest.Tree.t
         Deferred.t

    (** Tuple of account update tree, new deposits_processed, new remaining_deposits *)
    val step :
         deposits_processed:field
           (** The deposits processed at the time of the last step *)
      -> remaining_deposits:TR.t list
           (** The deposits yet to be processed, including new ones *)
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.Tree.t
         * field
         * TR.t list )
         Deferred.t

    (** Public key of inner account, closest point to 123456789 *)
    val public_key : Public_key.Compressed.t

    (** Account ID using public key *)
    val account_id : Account_id.t

    (** Initial state of inner account in new rollup *)
    val initial_account : Account.t
  end

  module Mocked : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    val step :
         t
      -> Public_key.Compressed.t
      -> field
      -> unit
      -> ( field Zkapp_statement.Poly.t
         * ( Account_update.Body.t
           * Zkapp_command.Digest.Account_update.t
           * ( Account_update.t
             , Zkapp_command.Digest.Account_update.t
             , Zkapp_command.Digest.Forest.t )
             Zkapp_command.Call_forest.t )
         * (Pickles_types.Nat.N1.n, Pickles_types.Nat.N1.n) Pickles.Proof.t )
         Deferred.t
  end

  module Outer : sig
    val vk : Verification_key_wire.t

    (** Account update for depositing *)
    val deposit :
         public_key:Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> recipient:Public_key.Compressed.t
      -> ( Account_update.t
         , Zkapp_command.Digest.Account_update.t
         , Zkapp_command.Digest.Forest.t )
         Zkapp_command.Call_forest.Tree.t
         Deferred.t

    (** Given a transition that includes an inner step made using
        the matching arguments (for deposits), produce a L1 account update for the rollup. *)
    val step :
         t (** The transition, must include Inner.step account update *)
      -> outer_public_key:Public_key.Compressed.t
           (** The public key on the outside *)
      -> old_all_deposits:field
           (** The deposits that were recorded at the last step *)
      -> new_deposits:TR.t list (** The new deposits since then *)
      -> withdrawals_processed:field
           (** The withdrawals processed at the last step *)
      -> remaining_withdrawals:TR.t list
           (** The remaining withdrawals, including new ones *)
      -> old_inner_ledger:Mina_ledger.Sparse_ledger.t
           (** Old sparse inner ledger including inner account *)
      -> new_inner_ledger:Mina_ledger.Sparse_ledger.t
           (** New sparse inner ledger including inner account *)
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.Tree.t
         * field
         * TR.t list )
         Deferred.t
    (** Tuple of account update tree, new withdrawals_processed, new remaining_withdrawals *)

    (** Create an account update update for deploying the zkapp, given a valid ledger for it. *)
    val deploy_exn : Mina_ledger.Ledger.t -> Account_update.Update.t

    (** Create an account update update for deploying the zkapp, given a ledger hash for it. If ledger hash is invalid rollup will be borked. *)
    val unsafe_deploy : Ledger_hash.t -> Account_update.Update.t
  end
end

module type Intf = sig
  type t

  val source_ledger : t -> Frozen_ledger_hash.t

  val target_ledger : t -> Frozen_ledger_hash.t

  val inner_pending_coinbase_init : Pending_coinbase.Stack.t

  val inner_pending_coinbase : Pending_coinbase.Stack.t

  val inner_state_body : Mina_state.Protocol_state.Body.Value.t

  (** Transfer request *)
  module TR : sig
    type t = { amount : Currency.Amount.t; recipient : Public_key.Compressed.t }
  end

  module Mocked_zkapp : sig
    module Deploy : sig
      val deploy :
           signer:Signature_lib.Keypair.t
        -> fee:Currency.Fee.t
        -> nonce:Account.Nonce.t
        -> zkapp:Signature_lib.Keypair.t
        -> vk:Side_loaded_verification_key.t
        -> initial_state:Frozen_ledger_hash.t
        -> Zkapp_command.t
    end
  end

  (** Module type for output of Make *)
  module type S = S with type t := t and module TR := TR

  (** Compiles circuits *)
  module Make (T : sig
    val tag : Transaction_snark.tag
  end) : S

  (** Public key of inner account, closest point to 123456789 *)
  val inner_public_key : Public_key.Compressed.t
end
