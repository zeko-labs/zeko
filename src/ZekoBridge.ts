import {
  AccountUpdate,
  Field,
  MerkleTree,
  MerkleWitness,
  Poseidon,
  Provable,
  PublicKey,
  SmartContract,
  State,
  Struct,
  UInt64,
  method,
  state,
} from 'snarkyjs';

export const TREE_HEIGHT = process.env.NODE_ENV !== 'test' ? 32 : 3;
export const TREE_CAPACITY = new MerkleTree(TREE_HEIGHT).leafCount;

export class WrappingRequest extends Struct({
  id: Field,
  amount: UInt64,
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

export class QueueWitness extends MerkleWitness(TREE_HEIGHT) {}

export class ZekoBridge extends SmartContract {
  @state(Field) treeRoot = State<Field>();
  @state(Field) firstIndex = State<Field>();
  @state(Field) counter = State<Field>();

  events = {
    'wrapping-request': WrappingRequest,
  };

  init() {
    super.init();

    this.treeRoot.set(new MerkleTree(TREE_HEIGHT).getRoot());
    this.firstIndex.set(Field(0));
    this.counter.set(Field(0));
  }

  lastIndex(): UInt64 {
    return new UInt64(this.counter.get().sub(1)).mod(new UInt64(TREE_CAPACITY));
  }

  @method createWrappingRequest(
    request: WrappingRequest,
    witness: QueueWitness
  ) {
    // Create account update that sends funds from the sender to the zkapp
    request.amount.assertGreaterThan(UInt64.zero);
    const senderUpdate = AccountUpdate.createSigned(this.sender);
    senderUpdate.send({ to: this, amount: request.amount });

    // Public inputs
    const treeRoot = this.treeRoot.getAndAssertEquals();
    const counter = this.counter.getAndAssertEquals();

    // // Assert we are not overflowing the list
    // counter.assertLessThan(TREE_CAPACITY);

    // Assert correct new request index
    counter.assertEquals(request.id);

    // Assert correct witness
    const requestIndex = new UInt64(request.id).mod(new UInt64(TREE_CAPACITY));

    new UInt64(witness.calculateIndex()).assertEquals(requestIndex);
    witness.calculateRoot(Field(0)).assertEquals(treeRoot);

    // Calculate new root
    const requestHash = request.hash();
    const newRoot = witness.calculateRoot(requestHash);

    this.treeRoot.set(newRoot);
    this.counter.set(counter.add(1));

    this.emitEvent('wrapping-request', request);
  }

  @method popWrappingRequest(requestHash: Field, witness: QueueWitness) {
    /**
     * some logic to check if the request can be removed
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

    const isThisEndOfList = firstIndex.equals(TREE_CAPACITY - 1n);
    this.firstIndex.set(
      Provable.if(isThisEndOfList, Field(0), firstIndex.add(1))
    );
  }
}
