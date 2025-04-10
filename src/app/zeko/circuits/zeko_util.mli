[@@@warning "-67"]

open Mina_base
open Snark_params.Tick
module Proof = Compile_simple.Proof

module Calls : sig
  type t =
    | []
    | ( :: ) of (Mina_base.Account_update.Checked.t * t) * t
    | Raw of Zkapp_call_forest.Checked.t

  val hash : t -> Zkapp_call_forest.Checked.t Checked.t
end

module Fine : sig
  module Case : sig
    type 'self t =
      | Whole : (('var, 't) Typ.t * 'var option) -> 'self t
      | Recursive : 'self -> 'self t
  end

  type t = [] : t | ( :: ) : t Case.t * t -> t
end

val var_to_precondition_fine :
     Fine.t
  -> Field.Var.t Zkapp_basic.Or_ignore.Checked.t Pickles_types.Vector.Vector_8.t

val var_to_app_state_fine :
     Fine.t
  -> Field.Var.t Zkapp_basic.Set_or_keep.Checked.t
     Pickles_types.Vector.Vector_8.t

val value_to_init_state : ('a, 'b) Typ.t -> 'b -> field Zkapp_state.V.t

val value_to_app_state :
  ('a, 'b) Typ.t -> 'b -> field Zkapp_basic.Set_or_keep.t Zkapp_state.V.t

val value_of_state : ('var, 'value) Typ.t -> field Zkapp_state.V.t -> 'value

val var_to_actions :
  ('var, 'value) Typ.t -> 'var -> Mina_base.Zkapp_account.Actions.var Checked.t

val var_to_hash :
  init:string -> ('var, 'value) Typ.t -> 'var -> Field.Var.t Checked.t

module F : sig
  type t = Pasta_bindings.Fp.t

  type var = Field.Var.t

  val typ : (var, t) Typ.t

  val pp : Format.formatter -> Pasta_bindings.Fp.t -> unit
end

module type SnarkType = sig
  type t

  type var

  val typ : (var, t) Typ.t
end

module SnarkList : functor
  (T : SnarkType)
  (Len : sig
     val length : int
   end)
  -> sig
  type t = T.t list

  type var = T.var list

  val typ : (var, t) Typ.t
end

module SnarkArray : functor
  (Inputs : sig
     module T : SnarkType

     val max_length : int

     val dummy_filler : T.t
   end)
  -> sig
  type t = Inputs.T.t list

  type var = { array : Inputs.T.var array; length : int V.t }

  val typ : (var, t) Typ.t
end

module type V_S = sig
  type t

  type var = t V.t

  val typ : (var, t) Typ.t
end

module Mk_V : functor
  (T : sig
     type t
   end)
  -> V_S with type t = T.t

module Proof_V : V_S with type t = Proof.t

module Proof_Option_V : V_S with type t = Proof.t option

module Boolean : sig
  include module type of Boolean

  type t = bool
end

val make_outputs :
     Mina_base.Account_update.Checked.t
  -> Calls.t
  -> ( Mina_base.Zkapp_statement.Checked.t
     * ( Mina_base.Account_update.Body.t
       * Mina_base.Zkapp_command.Digest.Account_update.t
       * ( Mina_base.Account_update.t
         , Mina_base.Zkapp_command.Digest.Account_update.t
         , Mina_base.Zkapp_command.Digest.Forest.t )
         Mina_base.Zkapp_command.Call_forest.t )
       V.t )
     Checked.t

val public_key_to_token_id_var :
  Mina_base_import.Public_key.Compressed.var -> Mina_base.Token_id.Checked.t

val authorization_vk_hash :
  F.var -> Mina_base.Account_update.Authorization_kind.Checked.t

val authorization_signed :
  unit -> Mina_base.Account_update.Authorization_kind.Checked.t

val assert_var : string -> (unit -> Boolean.var Checked.t) -> unit Checked.t

val default_account_update : Mina_base.Account_update.Checked.t

val ( let* ) : 'a Checked.t -> ('a -> 'b Checked.t) -> 'b Checked.t

val ( let*| ) : 'a Checked.t -> ('a -> 'b) -> 'b Checked.t

val ( let+ ) : 'a As_prover.t -> ('a -> 'b As_prover.t) -> 'b As_prover.t

val ( let+| ) : 'a As_prover.t -> ('a -> 'b) -> 'b As_prover.t

val ( let@ ) : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c

module Slot : sig
  include module type of Mina_numbers.Global_slot_since_genesis

  type var = Checked.t
end

module Slot_span : sig
  include module type of Mina_numbers.Global_slot_span

  type var = Checked.t
end

module Slot_range : sig
  type t = { lower : Slot.t; upper : Slot.t } [@@deriving snarky]

  module Checked : sig
    val to_valid_while :
      var -> Mina_base.Zkapp_precondition.Valid_while.Checked.t
  end

  val infinite : t
end

val assert_equal :
  ?label:string -> ('var, 't) Typ.t -> 'var -> 'var -> unit Checked.t

val assert_equal_safer :
  ?label:string -> ('var, 't) Typ.t -> 'var -> 'var -> 'var Checked.t

val var_equal : ('var, 't) Typ.t -> 'var -> 'var -> Boolean.Expr.t Checked.t

module Checked32 : sig
  include module type of Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

val push_actions_var :
  actions:Field.Var.t -> Field.Var.t -> Field.Var.t Checked.t

val token_owner_id : Account_id.t option -> Token_id.t

module Even_PC : sig
  type t = { public_key : F.t } [@@deriving snarky]

  val to_pc_var : var -> Import.Public_key.Compressed.var
end

val slot_range_intersection :
  Slot_range.var -> Slot_range.var -> Slot_range.var Checked.t

val accumulate : (('a -> unit) -> 'b Checked.t) -> ('b * 'a list) Checked.t
