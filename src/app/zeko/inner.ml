(** The rules for the inner account zkapp, that controls the money supply and transfers on the rollup *)

open Core_kernel
open Mina_base
open Signature_lib
open Zeko_util
open Snark_params.Tick

let public_key =
  let pk =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Field.of_int 123456789)
  in
  Public_key.compress pk

module State = struct
  type t = { outer_action_state : F.t  (** Action state of outer account. *) }
  [@@deriving snarky]

  let default : t = { outer_action_state = Zkapp_account.Actions.empty_state_element }

  let var_of_app_state (outer_action_state :: _ : F.var Zkapp_state.V.t) : var =
    { outer_action_state }

  let value_of_app_state (outer_action_state :: _ : F.t Zkapp_state.V.t) : t =
    { outer_action_state }
end

module Action = struct
  module Zkapp_call_forest = struct
    include Zkapp_call_forest

    type var = Checked.t
  end

  type t = { aux : F.t; children : Zkapp_call_forest.t } [@@deriving snarky]

  module State : sig
    include SnarkType

    val to_field_var : var -> F.var

    val of_field_var : F.var -> var
  end = struct
    type t = F.t

    type var = F.var

    let typ = F.typ

    let to_field_var x = x

    let of_field_var x = x
  end

  (* We discriminate between the actions by prefixing with a tag,
     even though we only have one case right now. We might have more
     in the future. *)

  let to_actions_var (x : var) =
    var_to_actions Typ.(F.typ * typ) (Field.of_int 0 |> Field.Var.constant, x)

  let push_var : var -> State.var -> State.var =
   fun x xs ->
    Zkapp_account.Actions.push_events_checked (State.to_field_var xs) (to_actions_var x)
    |> State.of_field_var
end
