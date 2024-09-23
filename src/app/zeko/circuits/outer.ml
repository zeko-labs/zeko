open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util
module PC = Signature_lib.Public_key.Compressed

module State = struct
  type t =
    { ledger_hash : Ledger_hash.t  (** The ledger hash of the rollup *)
    ; inner_action_state : Inner.Action.State.t
    ; sequencer : PC.t
    }
  [@@deriving snarky]

  type precondition =
    { ledger_hash : Ledger_hash.var option
          (** The ledger hash of the rollup *)
    ; inner_action_state : Inner.Action.State.var option
    ; sequencer : PC.var option
    }

  let to_precondition (p : precondition) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
    var_to_precondition_fine
      Var_to_precondition_fine.
        [ (Ledger_hash.typ, p.ledger_hash)
        ; (Inner.Action.State.typ, p.inner_action_state)
        ; (PC.typ, p.sequencer)
        ]
end

module Action = struct
  module Commit = struct
    type t =
      { ledger : Ledger_hash.t
      ; inner_action_state : F.t
      ; synchronized_outer_action_state : F.t
      ; sequencer : PC.t
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

  let dummy_slot_range : Slot_range.t = { lower = Slot.zero; upper = Slot.zero }

  let dummy_commit : Commit.t =
    { ledger = Outside_hash_image.t
    ; inner_action_state = Outside_hash_image.t
    ; synchronized_outer_action_state = Outside_hash_image.t
    ; sequencer = PC.empty
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

  let push_commit_var : Commit.var -> State.var -> State.var Checked.t =
   fun c xs ->
    let*| actions = commit_to_actions_var c in
    Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
    |> State.of_field_var

  let push_witness_var : Witness.var -> State.var -> State.var Checked.t =
   fun w xs ->
    let*| actions = witness_to_actions_var w in
    Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
    |> State.of_field_var

  let push_var : var -> State.var -> State.var Checked.t =
   fun x xs ->
    let*| actions = to_actions_var x in
    Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
    |> State.of_field_var
end
