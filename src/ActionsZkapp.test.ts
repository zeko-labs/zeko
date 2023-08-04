import {
  AccountUpdate,
  Bool,
  Field,
  Mina,
  PrivateKey,
  PublicKey,
  Reducer,
} from 'snarkyjs';
import {
  ActionsZkapp,
  CustomAction,
  CustomState,
  RollupActions,
  RollupState,
} from './ActionsZkapp';
import config from './config';
import { hashAction, updateActionsState } from './utils';

const getExpectedActionState = (
  actions: CustomAction[]
): { actionState: Field; state: CustomState } => {
  let actionState = Reducer.initialActionState;
  let state = new CustomState({ sum: Field(0), count: Field(0) });

  for (const action of actions) {
    actionState = updateActionsState(
      actionState,
      hashAction(action.toFields())
    );

    if (action.positive.toBoolean()) {
      state = new CustomState({
        sum: state.sum.add(action.amount),
        count: state.count.add(Field(1)),
      });
    } else {
      state = new CustomState({
        sum: state.sum.sub(action.amount),
        count: state.count.add(Field(1)),
      });
    }
  }

  return { actionState, state };
};

describe('ZekoBridge', () => {
  let senderKey: PrivateKey;
  let senderAddress: PublicKey;

  let zkappKey: PrivateKey;
  let zkappAddress: PublicKey;
  let zkapp: ActionsZkapp;

  let actions: CustomAction[] = [];

  beforeAll(async () => {
    if (config.PROOFS_ENABLED) await ActionsZkapp.compile();

    const Local = Mina.LocalBlockchain({
      proofsEnabled: config.PROOFS_ENABLED,
    });
    Mina.setActiveInstance(Local);

    ({ privateKey: senderKey, publicKey: senderAddress } =
      Local.testAccounts[0]);

    zkappKey = PrivateKey.random();
    zkappAddress = zkappKey.toPublicKey();
    zkapp = new ActionsZkapp(zkappAddress);
  });

  it('deploys the `ActionsZkapp` zkapp', async () => {
    const deployTx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress, 1);
      zkapp.deploy({ zkappKey });
    });

    await deployTx.prove();
    await deployTx.sign([senderKey, zkappKey]).send();

    expect(zkapp.state.get()).toEqual(
      new CustomState({ sum: Field(0), count: Field(0) })
    );
    expect(zkapp.actionState.get()).toEqual(Reducer.initialActionState);
  });

  it('dispatches actions', async () => {
    actions.push(
      new CustomAction({
        positive: Bool(true),
        amount: Field(5),
      })
    );

    actions.push(
      new CustomAction({
        positive: Bool(false),
        amount: Field(3),
      })
    );

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.dispatchAction(actions[0]);
      zkapp.dispatchAction(actions[1]);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();
  });

  it('rolls up actions with reducer', async () => {
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupWithReduce();
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    const { state: expectedState, actionState: expectedActionState } =
      getExpectedActionState(actions);

    expect(zkapp.state.get()).toEqual(expectedState);
    expect(zkapp.actionState.get()).toEqual(expectedActionState);
  });

  it('dispatches actions', async () => {
    actions.push(
      new CustomAction({
        positive: Bool(true),
        amount: Field(9),
      })
    );

    actions.push(
      new CustomAction({
        positive: Bool(false),
        amount: Field(3),
      })
    );

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.dispatchAction(actions[2]);
      zkapp.dispatchAction(actions[3]);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();
  });

  it('rolls up actions recursively', async () => {
    if (!config.PROOFS_ENABLED) return;

    await RollupActions.compile();

    const proof0 = await RollupActions.init(
      new RollupState(getExpectedActionState(actions.slice(0, -1))),
      new RollupState({
        state: zkapp.state.get(),
        actionState: zkapp.actionState.get(),
      }),
      actions[2]
    );

    const expectedRollupState = getExpectedActionState(actions);

    const proof1 = await RollupActions.step(
      new RollupState(expectedRollupState),
      proof0,
      actions[3]
    );

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupWithProof(proof1);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.state.get()).toEqual(expectedRollupState.state);
    expect(zkapp.actionState.get()).toEqual(expectedRollupState.actionState);
  });
});
