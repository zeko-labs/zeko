open Mina_base
open Zeko_util
open Snark_params.Tick
open Signature_lib

let inner_public_key = Zeko_circuits.Outer_rules.Inputs.inner_public_key

let inner_account_id = Account_id.create inner_public_key Token_id.default

type t = unit

let to_yojson _ = failwith "FIXME"

let of_yojson _ = failwith "FIXME"

module TR = struct
  type t =
    { amount : Currency.Amount.t
    ; recipient : Signature_lib.Public_key.Compressed.t
    }

  let to_yojson = to_yojson

  let of_yojson = of_yojson

  let to_actions _ = failwith "FIXME"
end

module type S = sig
  open Async

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
  end

  module Inner : sig
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

    (** Account ID using public key *)
    val account_id : Account_id.t

    (** Initial state of inner account in new rollup *)
    val initial_account : Account.t
  end

  module Outer : sig
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
  end
end

module Make (_ : sig end) : S = struct
  module Wrapper = struct
    let wrap _ = failwith "FIXME"

    let merge _ _ = failwith "FIXME"
  end

  module Outer = struct
    let submit_deposit ~outer_public_key:_ ~deposit:_ = failwith "FIXME"

    let process_withdrawal ~is_new:_ ~outer_public_key:_ ~pointer:_ ~before:_
        ~after:_ ~withdrawal:_ =
      failwith "FIXME"

    let step _ ~outer_public_key:_ ~new_deposits:_ ~unprocessed_deposits:_
        ~old_inner_ledger:_ ~new_inner_ledger:_ =
      failwith "FIXME"

    let deploy_exn _ = failwith "FIXME"
  end

  module Inner = struct
    let submit_withdrawal ~withdrawal:_ = failwith "FIXME"

    let process_deposit ~is_new:_ ~pointer:_ ~before:_ ~after:_ ~deposit:_ =
      failwith "FIXME"

    let step ~all_deposits:_ = failwith "FIXME"

    let account_id = inner_account_id

    let initial_account = Account.empty
  end
end
