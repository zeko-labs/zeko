open Core_kernel
open Async_kernel
open Mina_base
open Signature_lib

type t

val source_ledger : t -> Frozen_ledger_hash.t

val target_ledger : t -> Frozen_ledger_hash.t

module Make (T : sig
  val tag : Transaction_snark.tag
end) : sig
  module Wrapper : sig
    val wrap : Transaction_snark.t -> t Deferred.t

    val merge : t -> t -> t Deferred.t
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
         Zkapp_command.Call_forest.Tree.t
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
         Zkapp_command.Call_forest.Tree.t
         Deferred.t
  end
end
