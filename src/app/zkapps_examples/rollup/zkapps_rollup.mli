open Core_kernel
open Async_kernel

type t

val source_ledger : t -> Mina_base.Frozen_ledger_hash.t

val target_ledger : t -> Mina_base.Frozen_ledger_hash.t

module Make (T : sig
  val tag : Transaction_snark.tag
end) : sig
  module Wrapper : sig
    val wrap : Transaction_snark.t -> t Deferred.t

    val merge : t -> t -> t Deferred.t
  end
end
