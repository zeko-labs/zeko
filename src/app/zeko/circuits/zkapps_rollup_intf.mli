open Mina_base

module type S = sig

   
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

  module Inner : sig
    val vk : Pickles.Side_loaded.Verification_key.t

    (** Account update for withdrawing *)
    val submit_withdrawal : withdrawal:TR.t -> call_forest_tree Deferred.t

    val process_deposit :
         is_new:bool (** Has this recipient been deposited to before? *)
      -> pointer:field
      -> before:TR.t list (** deposits before last recorded from new to old *)
      -> after:TR.t list (** deposits after last recorded from new to old *)
      -> deposit:TR.t
      -> ([ `Pointer of field ] * call_forest) Deferred.t

    val step : all_deposits:field -> call_forest_tree Deferred.t

    (** Public key of inner account, closest point to 123456789 *)
    val public_key : Public_key.Compressed.t

    (** Account ID using public key *)
    val account_id : Account_id.t

    (** Initial state of inner account in new rollup *)
    val initial_account : Account.t
  end

  module Outer : sig
    val vk : Verification_key_wire.t

    (** Account update for depositing *)
    val submit_deposit :
         outer_public_key:Public_key.Compressed.t
      -> deposit:TR.t
      -> call_forest_tree Deferred.t

    val process_withdrawal :
         is_new:bool (** Has this recipient been withdrawn to before? *)
      -> outer_public_key:Public_key.Compressed.t
      -> pointer:field
      -> before:TR.t list
      -> after:TR.t list
      -> withdrawal:TR.t
      -> ([ `Pointer of field ] * call_forest) Deferred.t

    val step :
         t (** The transition, must include Inner.step account update *)
      -> outer_public_key:Public_key.Compressed.t
      -> new_deposits:TR.t list (** new deposits from new to old *)
      -> unprocessed_deposits:TR.t list
           (** from new deposits to the current action state *)
      -> old_inner_ledger:Mina_ledger.Sparse_ledger.t
           (** Old sparse inner ledger including inner account *)
      -> new_inner_ledger:Mina_ledger.Sparse_ledger.t
           (** New sparse inner ledger including inner account *)
      -> call_forest_tree Deferred.t

    (** Create an account update update for deploying the zkapp, given a valid ledger for it. *)
    val deploy_exn : Mina_ledger.Ledger.t -> Account_update.Update.t

    (** Create an account update update for deploying the zkapp, given a ledger hash for it. If ledger hash is invalid rollup will be borked. *)
    val unsafe_deploy : Ledger_hash.t -> Account_update.Update.t
  end
  
  *)
end

module Rollup_state = Rollup_state

module type Intf = sig
  val inner_pending_coinbase_init : Pending_coinbase.Stack.t

  val inner_pending_coinbase : Pending_coinbase.Stack.t

  val inner_state_body : Mina_state.Protocol_state.Body.Value.t

  val inner_account_id : Account_id.t

  (** Public key of inner account, closest point to 123456789 *)
  val inner_public_key : Signature_lib.Public_key.Compressed.t

  (** Module type for output of Make *)
  module type S = S

  (** Compiles circuits *)
  module Make (T : Transaction_snark.S) : S
  [@@warning "-67"]
end
