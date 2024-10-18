[@@@warning "-67"]

open Mina_base
open Snark_params.Tick
module Or_ignore = Zkapp_basic.Or_ignore
module Set_or_keep = Zkapp_basic.Set_or_keep

val attach_control_var :
     Mina_base.Account_update.Checked.t
  -> Mina_base.Zkapp_call_forest.Checked.account_update

val attach_control :
  Mina_base.Account_update.Body.t -> Mina_base.Zkapp_call_forest.account_update

val constraint_constants : Genesis_constants.Constraint_constants.t

val value_to_fields :
  ('var, 'value) Typ.t -> 'value -> Pasta_bindings.Fp.t array

val var_to_fields : ('var, 'value) Typ.t -> 'var -> Field.Var.t array

val var_to_field : ('var, 'value) Typ.t -> 'var -> Field.Var.t

val none_permissions : Mina_base.Permissions.t

type call_forest =
  ( Mina_base.Account_update.t
  , Mina_base.Zkapp_command.Digest.Account_update.t
  , Mina_base.Zkapp_command.Digest.Forest.t )
  Mina_base.Zkapp_command.Call_forest.t

type call_forest_tree =
  ( Mina_base.Account_update.t
  , Mina_base.Zkapp_command.Digest.Account_update.t
  , Mina_base.Zkapp_command.Digest.Forest.t )
  Mina_base.Zkapp_command.Call_forest.tree

module Calls : sig
  type t =
    | []
    | ( :: ) of (Mina_base.Account_update.Checked.t * t) * t
    | Raw of Zkapp_call_forest.Checked.t

  val hash : t -> Zkapp_call_forest.Checked.t Checked.t
end

val create_prover_value : 'a As_prover.t -> 'a Prover_value.t Checked.t

val make_outputs :
     Mina_base.Account_update.Checked.t
  -> Calls.t
  -> ( Mina_base.Zkapp_statement.Checked.t
     * ( Mina_base.Account_update.Body.t
       * Mina_base.Zkapp_command.Digest.Account_update.t
       * call_forest )
       As_prover.t )
     Checked.t

val mktree :
     Mina_base.Account_update.Body.t
     * 'a
     * ( ( Mina_base.Account_update.t
         , 'a
         , 'b )
         Mina_base.Zkapp_command.Call_forest.tree
       , 'b )
       Mina_base.With_stack_hash.t
       list
  -> ('c, 'd) Pickles.Proof.t
  -> ( Mina_base.Account_update.t
     , 'a
     , 'b )
     Mina_base.Zkapp_command.Call_forest.tree

val naive_hash_string_to_field : string -> Pasta_bindings.Fp.t

val var_to_state_generic :
     (Field.Var.t -> 'option)
  -> 'option
  -> ('var, 'value) Typ.t
  -> 'var
  -> 'option Pickles_types.Vector.Vector_8.t

val var_to_app_state :
     ('a, 'b) Typ.t
  -> 'a
  -> Field.Var.t Set_or_keep.Checked.t Pickles_types.Vector.Vector_8.t

val var_to_precondition :
     ('a, 'b) Typ.t
  -> 'a
  -> Field.Var.t Or_ignore.Checked.t Pickles_types.Vector.Vector_8.t

module Fine : sig
  module Case : sig
    type 'self t =
      | Whole : (('var, 't) Typ.t * 'var option) -> 'self t
      | Recursive : 'self -> 'self t
  end

  type t = [] : t | ( :: ) : t Case.t * t -> t
end

val var_to_precondition_fine :
  Fine.t -> Field.Var.t Or_ignore.Checked.t Pickles_types.Vector.Vector_8.t

val var_to_app_state_fine :
  Fine.t -> Field.Var.t Set_or_keep.Checked.t Pickles_types.Vector.Vector_8.t

val value_to_state_generic :
     (Pasta_bindings.Fp.t -> 'option)
  -> 'option
  -> ('var, 'value) Typ.t
  -> 'value
  -> 'option Pickles_types.Vector.Vector_8.t

val value_to_init_state :
  ('a, 'b) Typ.t -> 'b -> Pasta_bindings.Fp.t Pickles_types.Vector.Vector_8.t

val value_to_app_state :
     ('a, 'b) Typ.t
  -> 'b
  -> Pasta_bindings.Fp.t Set_or_keep.t Pickles_types.Vector.Vector_8.t

val var_to_actions :
  ('var, 'value) Typ.t -> 'var -> Mina_base.Zkapp_account.Actions.var Checked.t

val var_to_hash :
     init:field Random_oracle.State.t
  -> ('var, 'value) Typ.t
  -> 'var
  -> Field.Var.t Checked.t

val value_to_actions :
  ('var, 'value) Typ.t -> 'value -> Mina_base.Zkapp_account.Actions.t

module F : sig
  type t = Pasta_bindings.Fp.t

  type var = Field.Var.t

  val typ : (var, t) Typ.t

  val pp : Format.formatter -> Pasta_bindings.Fp.t -> unit
end

(** Proper reimplementation of Mina_base.Prover_value *)
module V : sig
  type 'a t

  val typ : ('a t, 'a) Typ.t

  val get : 'a t -> 'a As_prover.t

  val create : 'a As_prover.t -> 'a t Checked.t
end

module MkV : functor
  (T : sig
     type t
   end)
  -> sig
  type t = T.t

  type var = t V.t

  val typ : (var, t) Typ.t
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

module type V_S = sig
  type t

  type var = t V.t

  val typ : (var, t) Typ.t
end

module ProofV : V_S with type t = Mina_base.Proof.t

module ProofOptionV : V_S with type t = Mina_base.Proof.t option

module Boolean : sig
  include module type of Boolean

  type t = bool
end

module MkHandler : functor (Witness : SnarkType) -> sig
  type _ Snarky_backendless.Request.t +=
    | Witness : Witness.t Snarky_backendless.Request.t

  val handler :
       Witness.t
    -> Snarky_backendless.Request.request
    -> Snarky_backendless.Request.response

  val exists_witness : Witness.var Checked.t
end

val public_key_to_token_id_var :
  Mina_base_import.Public_key.Compressed.var -> Mina_base.Token_id.Checked.t

val authorization_vk_hash :
  F.var -> Mina_base.Account_update.Authorization_kind.Checked.t

val authorization_signed :
  unit -> Mina_base.Account_update.Authorization_kind.Checked.t

val time : string -> (unit -> 'a) -> 'a

val time_async :
  string -> (unit -> 'a Async_kernel.Deferred.t) -> 'a Async_kernel.Deferred.t

val assert_var : string -> (unit -> Boolean.var Checked.t) -> unit Checked.t

val default_account_update : Mina_base.Account_update.Checked.t

val ( let* ) : 'a Checked.t -> ('a -> 'b Checked.t) -> 'b Checked.t

val ( let*| ) : 'a Checked.t -> ('a -> 'b) -> 'b Checked.t

val ( let+ ) : 'a As_prover.t -> ('a -> 'b As_prover.t) -> 'b As_prover.t

val ( let+| ) : 'a As_prover.t -> ('a -> 'b) -> 'b As_prover.t

module Slot : sig
  include module type of Mina_numbers.Global_slot_since_genesis

  type var = Checked.t
end

module Slot_range : sig
  type t = { lower : Slot.t; upper : Slot.t } [@@deriving snarky]

  module Checked : sig
    val to_valid_while :
      var -> Mina_base.Zkapp_precondition.Valid_while.Checked.t
  end
end

val assert_equal :
  ?label:string -> ('var, 't) Typ.t -> 'var -> 'var -> unit Checked.t

module Checked32 : sig
  include module type of Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

(*
val compile_simple :
     ?override_wrap_domain:Pickles_base.Proofs_verified.t
  -> name:string
  -> main:
       (   unit
        -> ('output_var * 'aux As_prover.t)
           Checked.t )
  -> output:('output_var, 'output_t) Typ.t
  -> left_tag:('left_tag_var, 'left_tag_t, Pickles_types.Nat.N2.n, Pickles_types.Nat.N2.n) Pickles.Tag.t
  -> right_tag:('right_tag_var, 'right_tag_t, Pickles_types.Nat.N2.n, Pickles_types.Nat.N2.n) Pickles.Tag.t
  -> unit
  -> ('e, 'g, Pickles_types.Nat.N2.n, Pickles_types.Nat.N1.n) Pickles.Tag.t
     * Pickles.Cache_handle.t
     * (module Pickles.Proof_intf
          with type statement = 'g
           and type t = ( Pickles_types.Nat.N2.n
                        , Pickles_types.Nat.N2.n )
                        Pickles.Proof.t )
     * ( ('h * ('j * unit)) * unit
       , ('c * ('d * unit)) * unit
       , ('i * ('k * unit)) * unit
       , unit
       , ( 'g
         * 'f
         * (Pickles_types.Nat.N2.n, Pickles_types.Nat.N2.n) Pickles.Proof.t )
         Async_kernel.Deferred.t )
       Pickles.Provers.t
       *)
