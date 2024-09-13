open Zeko_util
open Snark_params.Tick

val public_key : Signature_lib.Public_key.Compressed.t

module State : sig
  type t = { outer_action_state : F.t  (** Action state of outer account. *) }
  [@@deriving snarky]

  val default : t

  val var_of_app_state : Field.Var.t Mina_base.Zkapp_state.V.t -> var

  val value_of_app_state : Field.t Mina_base.Zkapp_state.V.t -> t
end

module Action : sig
  type t = { aux : Field.t; children : Mina_base.Zkapp_call_forest.t }

  type var =
    { aux : Field.Var.t; children : Mina_base.Zkapp_call_forest.Checked.t }

  val typ : (var, t) Typ.t

  module State : sig
    include SnarkType

    val to_field_var : var -> Field.Var.t

    val of_field_var : Field.Var.t -> var
  end

  val to_actions_var : var -> Mina_base.Zkapp_account.Actions.var Checked.t

  val push_var : var -> State.var -> State.var Checked.t
end
