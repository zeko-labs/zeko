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

module Make (T : sig
  val tag : Transaction_snark.tag
end) : sig
  module Wrapper : sig
    val wrap : Transaction_snark.t -> t Deferred.t

    val merge : t -> t -> t Deferred.t

    val verify : t -> unit Or_error.t Deferred.t
  end

  module Inner : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    val action :
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
         deposits_processed:field
      -> remaining_deposits:TR.t list
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.t
         * field
         * TR.t list )
         Deferred.t
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
    val vk : Pickles.Side_loaded.Verification_key.t

    val action :
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
      -> ( ( Account_update.t
           , Zkapp_command.Digest.Account_update.t
           , Zkapp_command.Digest.Forest.t )
           Zkapp_command.Call_forest.t
         * field
         * TR.t list )
         Deferred.t
  end
end