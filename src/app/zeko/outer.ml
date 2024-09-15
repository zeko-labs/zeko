open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util
module PC = Signature_lib.Public_key.Compressed

(** Rules for outer account zkapp *)
module Make (Inputs : sig
  val macroslot_size : int
end)
(Macroslot : module type of Macroslot.Make (Inputs)) =
struct
  module State = struct
    (* NB! Don't change this, code depends on the layout, sorry *)
    type t =
      { ledger_hash : Ledger_hash.t  (** The ledger hash of the rollup *)
      ; inner_action_state : Inner.Action.State.t
      ; sequencer : PC.t
      ; macroslot : Macroslot.t
      ; bid_amount : Currency.Amount.t
      ; finalized : Boolean.t
            (** All withdrawals registered on the L2. They are paid out on the L1. *)
      }
    [@@deriving snarky]

    module Precondition = struct
      (* NB! Don't change this, code depends on the layout, sorry *)
      type t =
        { ledger_hash : Ledger_hash.var option
              (** The ledger hash of the rollup *)
        ; inner_action_state : Inner.Action.State.var option
        ; sequencer : PC.var option
        ; macroslot : Macroslot.var option
        ; bid_amount : Currency.Amount.var option
        ; finalized : Boolean.var option
              (** All withdrawals registered on the L2. They are paid out on the L1. *)
        }

      let to_precondition (t : t) : F.var Or_ignore.Checked.t Zkapp_state.V.t =
        var_to_precondition_fine
          Var_to_precondition_fine.
            [ (Ledger_hash.typ, t.ledger_hash)
            ; (Inner.Action.State.typ, t.inner_action_state)
            ; (PC.typ, t.sequencer)
            ; (Macroslot.typ, t.macroslot)
            ; (Currency.Amount.typ, t.bid_amount)
            ; (Boolean.typ, t.finalized)
            ]
    end
  end

  module Action = struct
    module Commit = struct
      type t =
        { ledger : Ledger_hash.t
        ; inner_action_state : F.t
        ; synchronized_outer_action_state : F.t
        ; sequencer : PC.t
        ; macroslot : Macroslot.t
        ; bid_amount : Currency.Amount.t
        ; slot_range : Slot_range.t
        }
      [@@deriving snarky]
    end

    module Bid = struct
      type t =
        { sequencer : PC.t
        ; bid_amount : Currency.Amount.t
        ; slot_range_bid_on : Slot_range.t
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

    type t = Commit of Commit.t | Bid of Bid.t | Witness of Witness.t

    module Repr = struct
      (* NB: If you add more cases, make sure to fix check in `typ`. *)
      type t =
        { is_commit : Boolean.t
        ; case_commit : Commit.t
        ; is_bid : Boolean.t
        ; case_bid : Bid.t
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
                Boolean.Assert.exactly_one
                  [ x.is_commit; x.is_bid; x.is_witness ] )
          }
    end

    type var = Repr.var

    let dummy_commit : Commit.t =
      { ledger = Outside_hash_image.t
      ; inner_action_state = Outside_hash_image.t
      ; synchronized_outer_action_state = Outside_hash_image.t
      ; sequencer = PC.empty
      ; macroslot = Macroslot.zero
      ; bid_amount = Currency.Amount.zero
      ; slot_range = { lower = Slot.zero; upper = Slot.zero }
      }

    let dummy_bid : Bid.t =
      { sequencer = PC.empty
      ; bid_amount = Currency.Amount.zero
      ; slot_range_bid_on = { lower = Slot.zero; upper = Slot.zero }
      ; slot_range = { lower = Slot.zero; upper = Slot.zero }
      }

    let dummy_witness : Witness.t =
      { aux = Field.zero
      ; children = Zkapp_call_forest.empty ()
      ; slot_range = { lower = Slot.zero; upper = Slot.zero }
      }

    let there : t -> Repr.t = function
      | Commit c ->
          { is_commit = true
          ; case_commit = c
          ; is_bid = false
          ; case_bid = dummy_bid
          ; is_witness = false
          ; case_witness = dummy_witness
          }
      | Bid b ->
          { is_commit = false
          ; case_commit = dummy_commit
          ; is_bid = true
          ; case_bid = b
          ; is_witness = false
          ; case_witness = dummy_witness
          }
      | Witness w ->
          { is_commit = false
          ; case_commit = dummy_commit
          ; is_bid = false
          ; case_bid = dummy_bid
          ; is_witness = true
          ; case_witness = w
          }
      | Time t ->
          { is_commit = false
          ; case_commit = dummy_commit
          ; is_bid = false
          ; case_bid = dummy_bid
          ; is_witness = false
          ; case_witness = dummy_witness
          }

    let back : Repr.t -> t =
     fun x ->
      match x with
      | { is_commit = true
        ; is_bid = false
        ; is_witness = false
        ; _
        } ->
          Commit x.case_commit
      | { is_commit = false
        ; is_bid = true
        ; is_witness = false
        ; _
        } ->
          Bid x.case_bid
      | { is_commit = false
        ; is_bid = false
        ; is_witness = true
        ; _
        } ->
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
        ~(case_bid : 'a) ~(case_witness : 'a) : 'a Checked.t =
      let switch_witness = case_witness in
      let* switch_bid =
        if_ x.is_bid ~typ ~then_:case_bid ~else_:switch_witness
      in
      if_ x.is_commit ~typ ~then_:case_commit ~else_:switch_bid

    (* We discriminate between the actions by prefixing with a tag,
       rather than using Repr.var as the action itself, which would be
       extremely wasteful and also incorrect, since we'd include the
       values for the unused fields in the hash. Those might not be initialized
       to their dummy values since they're not checked at all! *)

    let commit_to_actions_var (c : Commit.var) =
      var_to_actions Typ.(F.typ * Commit.typ) (Run.Field.of_int 0, c)

    let bid_to_actions_var (bid : Bid.var) =
      var_to_actions Typ.(F.typ * Bid.typ) (Run.Field.of_int 1, bid)

    let witness_to_actions_var (w : Witness.var) =
      var_to_actions Typ.(F.typ * Witness.typ) (Run.Field.of_int 2, w)

    let to_actions_var (x : var) =
      let* case_commit = commit_to_actions_var x.case_commit in
      let* case_bid = bid_to_actions_var x.case_bid in
      let* case_witness = witness_to_actions_var x.case_witness in
      switch x ~typ:Zkapp_account.Actions.typ ~case_commit ~case_bid
        ~case_witness

    let push_commit_var : Commit.var -> State.var -> State.var Checked.t =
     fun c xs ->
      let*| actions = commit_to_actions_var c in
      Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
      |> State.of_field_var

    let push_bid_var : Bid.var -> State.var -> State.var Checked.t =
     fun bid xs ->
      let*| actions = bid_to_actions_var bid in
      Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
      |> State.of_field_var

    let push_var : var -> State.var -> State.var Checked.t =
     fun x xs ->
      let*| actions = to_actions_var x in
      Zkapp_account.Actions.push_events_checked (State.to_field_var xs) actions
      |> State.of_field_var
  end
end
