open Core_kernel
open Async_kernel
open Mina_base
open Signature_lib
open Snark_params.Tick

type t

val source_ledger : t -> Frozen_ledger_hash.t

val target_ledger : t -> Frozen_ledger_hash.t

module TR : sig
  type t = { amount : Currency.Amount.t; recipient : Public_key.Compressed.t }
end

module Make (T : sig
  val tag : Transaction_snark.tag
end) : sig
  module Wrapper : sig
    val wrap :
         Transaction_snark.t
      -> Mina_numbers.Global_slot_since_genesis.t
      -> t Deferred.t

    val merge : t -> t -> t Deferred.t

    val verify : t -> unit Or_error.t Deferred.t
  end

  module Inner : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    (* Account update for withdrawing *)
    val withdraw :
         public_key:Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> recipient:Public_key.Compressed.t
      -> ( Account_update.t
         , Zkapp_command.Digest.Account_update.t
         , Zkapp_command.Digest.Forest.t )
         Zkapp_command.Call_forest.t
         Deferred.t

    (** Given the old state,
     a list of deposits,
     calculates a new account update for the inner *)
    val step :
         deposits_processed:field
      -> remaining_deposits:TR.t list
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.t
         * field
         * TR.t list )
         Deferred.t

    (* Public key of inner account, closest point to 123456789 *)
    val public_key : Public_key.Compressed.t

    (* Account ID using public key *)
    val account_id : Account_id.t

    (* Initial state of inner account in new rollup *)
    val initial_account : Account.t
  end

  module Outer : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    (* Account update for depositing *)
    val deposit :
         public_key:Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> recipient:Public_key.Compressed.t
      -> ( Account_update.t
         , Zkapp_command.Digest.Account_update.t
         , Zkapp_command.Digest.Forest.t )
         Zkapp_command.Call_forest.t
         Deferred.t

    (** Given the old state,
     a list of deposits to be made,
     a list of withdrawals to be made,
     a ledger transition,
     calculates a new account update for the inner and outer accounts *)
    val step :
         t
      -> public_key:Public_key.Compressed.t
      -> old_action_state:field
      -> new_actions:TR.t list
      -> withdrawals_processed:field
      -> remaining_withdrawals:TR.t list
      -> source_ledger:Mina_ledger.Sparse_ledger.t
      -> target_ledger:Mina_ledger.Sparse_ledger.t
      -> previous_global_slot_update:Mina_numbers.Global_slot_since_genesis.t
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.t
         * field
         * TR.t list )
         Deferred.t

    val step_without_transfers :
         t
      -> public_key:Public_key.Compressed.t
      -> previous_global_slot_update:Mina_numbers.Global_slot_since_genesis.t
      -> ( Account_update.t
         , Zkapp_command.Digest.Account_update.t
         , Zkapp_command.Digest.Forest.t )
         Zkapp_command.Call_forest.t
         Deferred.t

    val acceptable_global_slot_difference : Mina_numbers.Global_slot_span.t

    (* Create an account update update for deploying the zkapp, given a valid ledger for it. *)
    val deploy_update_exn : Mina_ledger.Ledger.t -> Account_update.Update.t

    (* Create an account update update for deploying the zkapp, given a ledger hash for it. If ledger hash is invalid rollup will be borked. *)
    val unsafe_deploy_update : Ledger_hash.t -> Account_update.Update.t

    val deploy_command_exn :
         signer:Signature_lib.Keypair.t
      -> fee:Currency.Fee.t
      -> nonce:Account.Nonce.t
      -> zkapp:Signature_lib.Keypair.t
      -> initial_ledger:Mina_ledger.Ledger.t
      -> Zkapp_command.t
  end
end
