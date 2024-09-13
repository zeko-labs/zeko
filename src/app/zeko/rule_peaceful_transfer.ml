(*
  include struct
    open struct
      module CalculateBid' = struct
        module Stmt = struct
          type t =
            { sequencer : PC.t
            ; bid_amount : CA.t
            ; macroslot : Macroslot.t
            ; action_state : Action.State.t
            }
          [@@deriving snarky]
        end

        module Elem = Action

        module BaseWitness = struct
          type t =
            { macroslot : Macroslot.t
            ; time_action : Action.t
            ; starting_point : Action.State.t
            }
          [@@deriving snarky]
        end

        let base : BaseWitness.var -> Stmt.var =
         fun { macroslot; time_action; starting_point } ->
          Boolean.(Assert.is_true @@ not time_action.is_witness) ;
          let slot_range =
            let if_ = if_var Slot_range.typ in
            if_ time_action.is_commit ~then_:time_action.case_commit.slot_range
              ~else_:
                (if_ time_action.is_bid ~then_:time_action.case_bid.slot_range
                   ~else_:time_action.case_time )
          in
          let bid_max_time_before =
            Mina_numbers.Global_slot_span.(
              constant typ @@ of_int bid_max_time_before)
          in
          (* Make sure that the slot_range for the time_action was made at a point at which
             it was too early to submit a bid. *)
          Boolean.Assert.is_true
            Slot.Checked.(
              slot_range.upper
              < (sub (Macroslot.lower_var macroslot) bid_max_time_before |> run)
              |> run) ;
          { sequencer = PC.(constant typ empty)
          ; bid_amount = CA.(constant typ zero)
          ; action_state = Action.push_var time_action starting_point
          ; macroslot
          }

        let step (x : Action.var) (acc : Stmt.var) =
          let is_better =
            Boolean.(
              x.is_bid
              && run CA.Checked.(x.case_bid.bid_amount > acc.bid_amount))
          in
          let bid_amount =
            CA.Checked.if_ is_better ~then_:x.case_bid.bid_amount
              ~else_:acc.bid_amount
            |> run
          in
          let sequencer =
            PC.Checked.if_ is_better ~then_:x.case_bid.sequencer
              ~else_:acc.sequencer
            |> run
          in
          let action_state = Action.push_var x acc.action_state in
          let macroslot = acc.macroslot in
          ({ sequencer; bid_amount; action_state; macroslot } : Stmt.var)
      end
    end

    module CalculateBid = Folder.Make (CalculateBid')
  end


    module Witness = struct
      type t =
        { winning_bid_proof : CalculateBid.t
        ; old_sequencer : PC.t
        ; ledger_hash : Ledger_hash.t
        ; inner_action_state : Inner.Action.State.t
        ; old_bid_amount : CA.t
        ; public_key : PC.t
        ; vk_hash : F.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let ({ winning_bid_proof
           ; old_sequencer
           ; ledger_hash
           ; inner_action_state
           ; old_bid_amount
           ; public_key
           ; vk_hash
           }
            : Witness.var ) =
        exists_witness ()
      in

      let ( ({ sequencer
             ; bid_amount
             ; macroslot
             ; action_state = outer_action_state
             } :
              CalculateBid.Stmt.var )
          , verify_winning_bid ) =
        CalculateBid.get winning_bid_proof
      in

      let prev_macroslot = Macroslot.dec macroslot in

      let account_update =
        { default_account_update with
          public_key
        ; authorization_kind = authorization_vk_hash vk_hash
        ; update =
            { default_account_update.update with
              app_state =
                State.(
                  var_to_app_state typ
                    ( { ledger_hash
                      ; inner_action_state
                      ; sequencer
                      ; macroslot
                      ; bid_amount
                      ; finalized = Boolean.false_
                      }
                      : var ))
            }
        ; preconditions =
            { default_account_update.preconditions with
              account =
                { default_account_update.preconditions.account with
                  state =
                    State.Precondition.to_precondition
                      { ledger_hash = Some ledger_hash
                      ; inner_action_state = Some inner_action_state
                      ; macroslot = Some prev_macroslot
                      ; sequencer = Some old_sequencer
                      ; bid_amount = Some old_bid_amount
                      ; finalized = Some Boolean.true_
                      }
                ; action_state =
                    Or_ignore.Checked.make_unsafe Boolean.true_
                      (Action.State.to_field_var outer_action_state)
                }
            ; valid_while =
                Slot_range.Checked.to_valid_while
                  { lower = Macroslot.lower_var prev_macroslot
                  ; upper = Macroslot.upper_var macroslot
                  }
            }
        }
      in
      let scale : int -> CA.var -> CA.var =
       fun factor x ->
        Field.mul (CA.Checked.to_field x) (Field.of_int factor)
        |> CA.Checked.of_field
      in
      let refund_amount =
        scale (bid_deposit_factor - 1) old_bid_amount |> CAS.Checked.of_unsigned
      in
      let refund_zeko =
        { default_account_update with
          token_id = zeko_token_id
        ; public_key = old_sequencer
        ; balance_change = refund_amount
        ; may_use_token = May_use_token.Checked.constant Parents_own_token
        }
      in
      let deposit_amount =
        scale bid_deposit_factor bid_amount |> CAS.Checked.of_unsigned
      in
      let deposit_zeko =
        { default_account_update with
          token_id = zeko_token_id
        ; public_key = sequencer
        ; balance_change = CAS.Checked.negate deposit_amount
        ; may_use_token = May_use_token.Checked.constant Parents_own_token
        }
      in
      let store_zeko =
        { default_account_update with
          token_id = zeko_token_id
        ; public_key
        ; balance_change =
            CAS.Checked.(add deposit_amount (negate refund_amount)) |> run
        ; may_use_token = May_use_token.Checked.constant Parents_own_token
        }
      in
      let zeko_token_owner =
        { default_account_update with
          public_key = constant PC.typ zeko_token_owner
        ; authorization_kind =
            zeko_token_owner_vk_hash |> Field.constant |> authorization_vk_hash
            (* FIXME add call_data *)
        }
      in

      (* Assemble some stuff to help the prover and calculate public output *)
      let public_output, auxiliary_output =
        make_outputs account_update
          [ ( zeko_token_owner
            , [ (deposit_zeko, []); (store_zeko, []); (refund_zeko, []) ] )
          ]
      in
      Pickles.Inductive_rule.
        { previous_proof_statements = [ verify_winning_bid ]
        ; public_output
        ; auxiliary_output
        }

    let rule _tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step"
      ; prevs = [ force CalculateBid.tag ]
      ; main
      ; feature_flags
      }
            *)
