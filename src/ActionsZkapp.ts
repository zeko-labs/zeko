import {
  Bool,
  Experimental,
  Field,
  FlexibleProvablePure,
  Poseidon,
  Provable,
  Reducer,
  SelfProof,
  SmartContract,
  State,
  Struct,
  method,
  state,
} from 'snarkyjs';
import { hashAction, updateActionsState } from './utils';

export class CustomAction extends Struct({
  positive: Bool,
  amount: Field,
}) {
  hash(): Field {
    return Poseidon.hash(CustomAction.toFields(this));
  }

  toFields(): Field[] {
    return CustomAction.toFields(this);
  }
}

export class CustomState extends Struct({
  sum: Field,
  count: Field,
}) {
  hash(): Field {
    return Poseidon.hash(CustomState.toFields(this));
  }
}

export class RollupState extends Struct({
  state: CustomState,
  actionState: Field,
}) {}

export const RollupActions = Experimental.ZkProgram({
  publicInput: RollupState,

  methods: {
    init: {
      privateInputs: [RollupState, CustomAction],

      method(
        nextState: RollupState,
        oldState: RollupState,
        action: CustomAction
      ) {
        nextState.actionState.assertEquals(
          updateActionsState(
            oldState.actionState,
            hashAction(action.toFields())
          )
        );

        nextState.state.count.assertEquals(oldState.state.count.add(1));
        nextState.state.sum.assertEquals(
          Provable.if(
            action.positive,
            oldState.state.sum.add(action.amount),
            oldState.state.sum.sub(action.amount)
          )
        );
      },
    },

    step: {
      privateInputs: [SelfProof, CustomAction],

      method(
        nextState: RollupState,
        prevProof: SelfProof<RollupState, FlexibleProvablePure<unknown>>,
        action: CustomAction
      ) {
        prevProof.verify();

        nextState.actionState.assertEquals(
          updateActionsState(
            prevProof.publicInput.actionState,
            hashAction(action.toFields())
          )
        );

        nextState.state.count.assertEquals(
          prevProof.publicInput.state.count.add(1)
        );
        nextState.state.sum.assertEquals(
          Provable.if(
            action.positive,
            prevProof.publicInput.state.sum.add(action.amount),
            prevProof.publicInput.state.sum.sub(action.amount)
          )
        );
      },
    },
  },
});

const RollupProof_ = Experimental.ZkProgram.Proof(RollupActions);
export class RollupProof extends RollupProof_ {}

export class ActionsZkapp extends SmartContract {
  reducer = Reducer({ actionType: CustomAction });

  @state(CustomState) state = State<CustomState>();
  @state(Field) actionState = State<Field>();

  init() {
    super.init();

    this.state.set(new CustomState({ sum: Field(0), count: Field(0) }));
    this.actionState.set(Reducer.initialActionState);
  }

  @method dispatchAction(action: CustomAction) {
    this.reducer.dispatch(action);
  }

  @method rollupWithReduce() {
    const state = this.state.getAndAssertEquals();
    const actionState = this.actionState.getAndAssertEquals();

    const pendingActions = this.reducer.getActions({
      fromActionState: actionState,
    });

    const { state: newState, actionState: newActionState } =
      this.reducer.reduce(
        pendingActions,
        CustomState,
        (state, action) => {
          return {
            sum: Provable.if(
              action.positive,
              state.sum.add(action.amount),
              state.sum.sub(action.amount)
            ),
            count: state.count.add(1),
          };
        },
        { state, actionState }
      );

    this.state.set(new CustomState(newState));
    this.actionState.set(newActionState);
  }

  @method rollupWithProof(proof: RollupProof) {
    this.account.actionState.assertEquals(proof.publicInput.actionState);

    proof.verify();

    this.state.set(proof.publicInput.state);
    this.actionState.set(proof.publicInput.actionState);
  }
}
