import {
  AccountUpdate,
  Bool,
  Experimental,
  Field,
  FlexibleProvablePure,
  MerkleTree,
  MerkleWitness,
  Poseidon,
  PrivateKey,
  Provable,
  PublicKey,
  Reducer,
  SelfProof,
  SmartContract,
  State,
  Struct,
  TokenId,
  UInt64,
  method,
  state,
} from 'snarkyjs';
import { hashAction, updateActionsState } from './utils';

export const TREE_HEIGHT = process.env.NODE_ENV !== 'test' ? 32 : 3;
export const TREE_CAPACITY = new MerkleTree(TREE_HEIGHT).leafCount;

export class WrappingRequest extends Struct({
  id: Field,
  amount: UInt64,
  tokenId: Field,
  receiver: PublicKey,
}) {
  hash(): Field {
    return Poseidon.hash(WrappingRequest.toFields(this));
  }

  index() {
    return this.id.toBigInt() % TREE_CAPACITY;
  }

  static buildMerkleTree(requests: WrappingRequest[]): MerkleTree {
    const tree = new MerkleTree(TREE_HEIGHT);

    requests.forEach((request) => {
      tree.setLeaf(request.index(), request.hash());
    });

    return tree;
  }
}

export class WrappingRequestAction extends Struct({
  amount: UInt64,
  tokenId: Field,
  receiver: PublicKey,
}) {
  hash(): Field {
    return hashAction(WrappingRequestAction.toFields(this));
  }

  static getActionState(
    actions: WrappingRequestAction[],
    initial = Reducer.initialActionState
  ): Field {
    return actions.reduce(
      (acc, action) => updateActionsState(acc, action.hash()),
      initial
    );
  }

  static async buildReducerProof(
    actions: WrappingRequestAction[],
    oldState: ReducerState,
    tree: MerkleTree
  ): Promise<ActionsReducerProof> {
    const actionsCopy = actions.slice();
    let proof: ActionsReducerProof | undefined = undefined;
    let publicInput = new ReducerPublicInput({
      currentState: new ReducerState(oldState),
      initialState: new ReducerState(oldState),
    });

    while (actionsCopy.length > 0) {
      const batch = actionsCopy.splice(0, BATCH_SIZE);

      const witnesses = batch.map((action) => {
        const request = new WrappingRequest({
          id: publicInput.currentState.counter,
          amount: action.amount,
          tokenId: action.tokenId,
          receiver: action.receiver,
        });

        const w = new QueueWitness(tree.getWitness(request.index()));
        tree.setLeaf(request.index(), request.hash());

        publicInput = new ReducerPublicInput({
          currentState: new ReducerState({
            actionState: updateActionsState(
              publicInput.currentState.actionState,
              action.hash()
            ),
            treeRoot: tree.getRoot(),
            counter: publicInput.currentState.counter.add(1),
          }),
          initialState: publicInput.initialState,
        });

        return new OptionalQueueWitness({
          action,
          w,
          exists: Bool(true),
        });
      });

      // Fill with empty witnesses
      for (let i = witnesses.length; i < BATCH_SIZE; i++) {
        witnesses.push(OptionalQueueWitness.empty());
      }

      proof =
        proof === undefined
          ? await ActionsRecursiveReducer.first(
              publicInput,
              new ActionsBatch({ witnesses })
            )
          : await ActionsRecursiveReducer.step(
              publicInput,
              proof,
              new ActionsBatch({ witnesses })
            );
    }

    if (!proof) {
      throw new Error('No proof');
    }

    return proof;
  }
}

export class QueueWitness extends MerkleWitness(TREE_HEIGHT) {}

export class OptionalQueueWitness extends Struct({
  action: WrappingRequestAction,
  w: QueueWitness,
  exists: Bool,
}) {
  static empty(): OptionalQueueWitness {
    return new OptionalQueueWitness({
      action: new WrappingRequestAction({
        amount: UInt64.zero,
        tokenId: Field(0),
        receiver: PrivateKey.random().toPublicKey(),
      }),
      w: new QueueWitness(new MerkleTree(TREE_HEIGHT).getWitness(0n)),
      exists: Bool(false),
    });
  }
}

export const BATCH_SIZE = process.env.NODE_ENV !== 'test' ? 32 : 2;

export class ActionsBatch extends Struct({
  witnesses: new Array(BATCH_SIZE).fill(OptionalQueueWitness),
}) {
  reduce(oldState: ReducerState): ReducerState {
    let stateAux = new ReducerState(oldState);

    for (let i = 0; i < BATCH_SIZE; i++) {
      const {
        action,
        w,
        exists,
      }: {
        action: WrappingRequestAction;
        w: QueueWitness;
        exists: Bool;
      } = this.witnesses[i];

      const requestIndex = new UInt64(stateAux.counter).mod(
        new UInt64(TREE_CAPACITY)
      );
      const request = new WrappingRequest({
        id: stateAux.counter,
        amount: action.amount,
        tokenId: action.tokenId,
        receiver: action.receiver,
      });

      // Verify correct witness
      // if it doesn't exist, compare same values
      Provable.if(
        exists,
        new UInt64(w.calculateIndex()),
        requestIndex
      ).assertEquals(requestIndex);

      Provable.if(
        exists,
        w.calculateRoot(Field(0)),
        stateAux.treeRoot
      ).assertEquals(stateAux.treeRoot);

      // Update state if exists
      stateAux = new ReducerState({
        actionState: Provable.if(
          exists,
          updateActionsState(stateAux.actionState, action.hash()),
          stateAux.actionState
        ),
        treeRoot: Provable.if(
          exists,
          w.calculateRoot(request.hash()),
          stateAux.treeRoot
        ),
        counter: Provable.if(exists, stateAux.counter.add(1), stateAux.counter),
      });
    }

    return stateAux;
  }
}

export class ReducerState extends Struct({
  actionState: Field,
  treeRoot: Field,
  counter: Field,
}) {
  assertEquals(other: ReducerState) {
    this.actionState.assertEquals(other.actionState);
    this.treeRoot.assertEquals(other.treeRoot);
    this.counter.assertEquals(other.counter);
  }
}

export class ReducerPublicInput extends Struct({
  currentState: ReducerState,
  initialState: ReducerState,
}) {}

export const ActionsRecursiveReducer = Experimental.ZkProgram({
  publicInput: ReducerPublicInput,

  methods: {
    first: {
      privateInputs: [ActionsBatch],

      method(publicInput: ReducerPublicInput, batch: ActionsBatch) {
        const newState = batch.reduce(publicInput.initialState);
        publicInput.currentState.assertEquals(newState);
      },
    },

    step: {
      privateInputs: [SelfProof, ActionsBatch],

      method(
        publicInput: ReducerPublicInput,
        prevProof: SelfProof<ReducerPublicInput, FlexibleProvablePure<unknown>>,
        batch: ActionsBatch
      ) {
        prevProof.verify();

        publicInput.initialState.assertEquals(
          prevProof.publicInput.initialState
        );
        const newState = batch.reduce(prevProof.publicInput.currentState);
        publicInput.currentState.assertEquals(newState);
      },
    },
  },
});

const ActionsReducerProof_ = Experimental.ZkProgram.Proof(
  ActionsRecursiveReducer
);
export class ActionsReducerProof extends ActionsReducerProof_ {}

export class ZekoBridge extends SmartContract {
  @state(Field) treeRoot = State<Field>();
  @state(Field) firstIndex = State<Field>();
  @state(Field) counter = State<Field>();
  @state(Field) actionState = State<Field>();

  reducer = Reducer({ actionType: WrappingRequestAction });

  events = {
    'wrapping-request': WrappingRequest,
  };

  init() {
    super.init();

    this.treeRoot.set(new MerkleTree(TREE_HEIGHT).getRoot());
    this.firstIndex.set(Field(0));
    this.counter.set(Field(0));
    this.actionState.set(Reducer.initialActionState);
  }

  lastIndex(): UInt64 {
    return new UInt64(this.counter.get().sub(1)).mod(new UInt64(TREE_CAPACITY));
  }

  @method createMinaWrappingRequest(request: WrappingRequestAction) {
    // Assert the request is for Mina and not custom token
    request.tokenId.assertEquals(TokenId.default);

    // Create an account update that sends funds from the sender to the zkapp
    request.amount.assertGreaterThan(UInt64.zero);
    const senderUpdate = AccountUpdate.createSigned(this.sender);
    senderUpdate.send({ to: this, amount: request.amount });

    // Dispatch action
    this.reducer.dispatch(request);
  }

  @method rollupRequestsWithProof(proof: ActionsReducerProof) {
    const treeRoot = this.treeRoot.getAndAssertEquals();
    const counter = this.counter.getAndAssertEquals();
    const actionState = this.actionState.getAndAssertEquals();

    treeRoot.assertEquals(proof.publicInput.initialState.treeRoot);
    counter.assertEquals(proof.publicInput.initialState.counter);
    actionState.assertEquals(proof.publicInput.initialState.actionState);

    this.account.actionState.assertEquals(
      proof.publicInput.currentState.actionState
    );

    proof.verify();

    this.actionState.set(proof.publicInput.currentState.actionState);
    this.treeRoot.set(proof.publicInput.currentState.treeRoot);
    this.counter.set(proof.publicInput.currentState.counter);
  }

  @method popWrappingRequest(requestHash: Field, witness: QueueWitness) {
    /**
     * some logic to check if the request can be popped
     */

    // Public inputs
    const treeRoot = this.treeRoot.getAndAssertEquals();
    const firstIndex = this.firstIndex.getAndAssertEquals();

    // Assert we are not popping empty queue
    requestHash.assertNotEquals(Field(0));

    // Assert correct witness
    witness.calculateIndex().assertEquals(firstIndex);
    witness.calculateRoot(requestHash).assertEquals(treeRoot);

    // Calculate new root
    const newRoot = witness.calculateRoot(Field(0));
    this.treeRoot.set(newRoot);

    // If this is the last leaf in the tree, reset the first index to 0
    const isThisEndOfList = firstIndex.equals(TREE_CAPACITY - 1n);
    this.firstIndex.set(
      Provable.if(isThisEndOfList, Field(0), firstIndex.add(1))
    );
  }
}
