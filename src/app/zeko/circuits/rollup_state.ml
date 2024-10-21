open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util
module PC = Signature_lib.Public_key.Compressed
open Checked.Let_syntax

let push_events_checked state actions =
  make_checked (fun () ->
      Zkapp_account.Actions.push_events_checked state actions )

module type Action_state_type = sig
  include SnarkType

  val empty : t

  val raw_var : var -> F.var

  val raw : t -> F.t

  val unsafe_var_of_field : F.var -> var

  val unsafe_value_of_field : F.t -> t

  module With_length : sig
    type without_length := t

    type without_length_var := var

    include SnarkType

    val empty : t

    val raw_var : var -> F.var

    val raw : t -> F.t

    val state_var : var -> without_length_var

    val state : t -> without_length

    val length_var : var -> Checked32.var

    val length : t -> Checked32.t

    val unsafe_var_of_fields :
      state:without_length_var -> length:Checked32.var -> var

    val unsafe_value_of_fields : state:without_length -> length:Checked32.t -> t

    type fine =
      { state : without_length_var option; length : Checked32.var option }

    val fine : fine -> Fine.t
  end
end

module Make_typed_action_state () : Action_state_type = struct
  type t = F.t

  type var = F.var

  let typ = F.typ

  let empty : t = Zkapp_account.Actions.empty_state_element

  let raw_var x = x

  let raw x = x

  let unsafe_var_of_field x = x

  let unsafe_value_of_field x = x

  module With_length = struct
    type t = { state : F.t; length : Checked32.t } [@@deriving snarky]

    let empty : t = { state = empty; length = Checked32.zero }

    let raw_var { state; length = _ } = state

    let raw ({ state; length = _ } : t) = state

    let state_var = raw_var

    let state = raw

    let length_var { length; state = _ } = length

    let length ({ length; state = _ } : t) = length

    let unsafe_var_of_fields ~state ~length = { state; length }

    let unsafe_value_of_fields ~state ~length : t = { state; length }

    type fine = { state : F.var option; length : Checked32.var option }

    (* NB! This will warn you if add a field to `t` without fixing it here.
       Make sure you add the field to `fine` too.
    *)
    let _ = function ({ state = _; length = _ } : t) -> ()
    (* Did you read the above? *)

    let fine ({ state; length } : fine) : Fine.t =
      [ Whole (F.typ, state); Whole (Checked32.typ, length) ]
  end
end

module Inner_action_state = Make_typed_action_state ()

module Outer_action_state = Make_typed_action_state ()

module Inner = struct
  let public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x @@ Field.of_int 123456789)
    in
    Signature_lib.Public_key.compress pk

  module State = struct
    type t =
      { outer_action_state : Outer_action_state.With_length.t
            (** Action state of outer account. *)
      }
    [@@deriving snarky]

    let default : t =
      { outer_action_state = Outer_action_state.With_length.empty }

    let var_of_app_state (state :: length :: _ : F.var Zkapp_state.V.t) : var =
      let state = Outer_action_state.unsafe_var_of_field state in
      let length = Checked32.Checked.Unsafe.of_field length in
      { outer_action_state =
          Outer_action_state.With_length.unsafe_var_of_fields ~state ~length
      }

    let value_of_app_state (state :: length :: _ : F.t Zkapp_state.V.t) : t =
      let state = Outer_action_state.unsafe_value_of_field state in
      let length = Field.to_string length |> Checked32.of_string in
      { outer_action_state =
          Outer_action_state.With_length.unsafe_value_of_fields ~state ~length
      }

    type fine = { outer_action_state : Outer_action_state.With_length.fine }

    (* NB! This will warn you if add a field to `t` without fixing it here.
       Make sure you add the field to `fine` too.
    *)
    let _ = function ({ outer_action_state = _ } : t) -> ()
    (* Did you read the above? *)

    let fine (p : fine) : Fine.t =
      [ Recursive (Outer_action_state.With_length.fine p.outer_action_state) ]
  end

  module Action = struct
    module Zkapp_call_forest = struct
      include Zkapp_call_forest

      type var = Checked.t
    end

    type t = { aux : F.t; children : Zkapp_call_forest.t } [@@deriving snarky]

    (* We discriminate between the actions by prefixing with a tag,
       even though we only have one case right now. We might have more
       in the future. *)

    let to_actions_var (x : var) =
      var_to_actions Typ.(F.typ * typ) (Field.of_int 0 |> Field.Var.constant, x)

    let push_var :
        var -> Inner_action_state.var -> Inner_action_state.var Checked.t =
     fun x xs ->
      let* actions = to_actions_var x in
      push_events_checked (Inner_action_state.raw_var xs) actions
      >>| Inner_action_state.unsafe_var_of_field
  end
end

module Outer = struct
  module State = struct
    (* NB: change precondition code too if you change this *)
    type t =
      { pause_key : PC.t
      ; paused : Boolean.t
      ; ledger_hash : Ledger_hash.t  (** The ledger hash of the rollup *)
      ; inner_action_state : Inner_action_state.With_length.t
      ; sequencer : PC.t
      }
    [@@deriving snarky]

    type fine =
      { pause_key : PC.var option
      ; paused : Boolean.var option
      ; ledger_hash : Ledger_hash.var option
      ; inner_action_state : Inner_action_state.With_length.fine
      ; sequencer : PC.var option
      }

    (* NB! This will warn you if add a field to `t` without fixing it here.
       Make sure you add the field to `fine` too.
    *)
    let _ = function
      | ({ pause_key = _
         ; paused = _
         ; ledger_hash = _
         ; inner_action_state = _
         ; sequencer = _
         } :
          t ) ->
          ()
    (* Did you read the above? *)

    let fine
        ({ pause_key; paused; ledger_hash; inner_action_state; sequencer } :
          fine ) : Fine.t =
      [ Whole (PC.typ, pause_key)
      ; Whole (Boolean.typ, paused)
      ; Whole (Ledger_hash.typ, ledger_hash)
      ; Recursive (Inner_action_state.With_length.fine inner_action_state)
      ; Whole (PC.typ, sequencer)
      ]
  end

  module Action = struct
    module Commit = struct
      type t =
        { ledger : Ledger_hash.t
        ; inner_action_state : Inner_action_state.With_length.t
        ; synchronized_outer_action_state : Outer_action_state.With_length.t
        ; slot_range : Slot_range.t
        }
      [@@deriving snarky]
    end

    module Zkapp_call_forest = struct
      include Zkapp_call_forest

      type var = Checked.t
    end

    module Witness = struct
      type t =
        { aux : F.t; children : Zkapp_call_forest.t; slot_range : Slot_range.t }
      [@@deriving snarky]
    end

    type t = Commit of Commit.t | Witness of Witness.t

    module Repr = struct
      (* NB: If you add more cases, make sure to fix check in `typ`. *)
      type t =
        { is_commit : Boolean.t
        ; case_commit : Commit.t
        ; is_witness : Boolean.t
        ; case_witness : Witness.t
        }
      [@@deriving snarky]

      let typ =
        let (Typ typ') = typ in
        let open Snark_params.Tick in
        Typ.Typ
          { typ' with
            check =
              (fun x ->
                let%bind () = typ'.check x in
                Boolean.Assert.exactly_one [ x.is_commit; x.is_witness ] )
          }
    end

    type var = Repr.var

    let dummy_slot_range : Slot_range.t =
      { lower = Slot.zero; upper = Slot.zero }

    let dummy_commit : Commit.t =
      { ledger = Outside_hash_image.t
      ; inner_action_state = Inner_action_state.With_length.empty
      ; synchronized_outer_action_state = Outer_action_state.With_length.empty
      ; slot_range = dummy_slot_range
      }

    let dummy_witness : Witness.t =
      { aux = Field.zero
      ; children = Zkapp_call_forest.empty ()
      ; slot_range = dummy_slot_range
      }

    let there : t -> Repr.t = function
      | Commit c ->
          { is_commit = true
          ; case_commit = c
          ; is_witness = false
          ; case_witness = dummy_witness
          }
      | Witness w ->
          { is_commit = false
          ; case_commit = dummy_commit
          ; is_witness = true
          ; case_witness = w
          }

    let back : Repr.t -> t =
     fun x ->
      match x with
      | { is_commit = true; is_witness = false; _ } ->
          Commit x.case_commit
      | { is_commit = false; is_witness = true; _ } ->
          Witness x.case_witness
      | _ ->
          failwith "should be unreachable"

    let typ : (var, t) Typ.t = Typ.transport ~there ~back Repr.typ

    let switch (x : var) ~(typ : ('a, 't) Typ.t) ~(case_commit : 'a)
        ~(case_witness : 'a) : 'a Checked.t =
      if_ x.is_commit ~typ ~then_:case_commit ~else_:case_witness

    (* We discriminate between the actions by prefixing with a tag,
       rather than using Repr.var as the action itself, which would be
       extremely wasteful and also incorrect, since we'd include the
       values for the unused fields in the hash. Those might not be initialized
       to their dummy values since they're not checked at all! *)

    let commit_to_actions_var (c : Commit.var) =
      var_to_actions Typ.(F.typ * Commit.typ) (Run.Field.of_int 0, c)

    let witness_to_actions_var (w : Witness.var) =
      var_to_actions Typ.(F.typ * Witness.typ) (Run.Field.of_int 1, w)

    let to_actions_var (x : var) =
      let* case_commit = commit_to_actions_var x.case_commit in
      let* case_witness = witness_to_actions_var x.case_witness in
      switch x ~typ:Zkapp_account.Actions.typ ~case_commit ~case_witness

    let push_commit_var :
        Commit.var -> Outer_action_state.var -> Outer_action_state.var Checked.t
        =
     fun c xs ->
      let* actions = commit_to_actions_var c in
      push_events_checked (Outer_action_state.raw_var xs) actions
      >>| Outer_action_state.unsafe_var_of_field

    let push_witness_var :
           Witness.var
        -> Outer_action_state.var
        -> Outer_action_state.var Checked.t =
     fun w xs ->
      let* actions = witness_to_actions_var w in
      push_events_checked (Outer_action_state.raw_var xs) actions
      >>| Outer_action_state.unsafe_var_of_field

    let push_var :
        var -> Outer_action_state.var -> Outer_action_state.var Checked.t =
     fun x xs ->
      let* actions = to_actions_var x in
      push_events_checked (Outer_action_state.raw_var xs) actions
      >>| Outer_action_state.unsafe_var_of_field
  end
end
