import {
  AccountUpdate,
  Field,
  MerkleTree,
  MerkleWitness,
  Poseidon,
  PublicKey,
  SmartContract,
  State,
  Struct,
  UInt64,
  method,
  state,
} from 'snarkyjs';

export const TREE_HEIGHT = 64;

export class WrappingRequest extends Struct({
  index: Field,
  amount: UInt64,
  receiver: PublicKey,
}) {
  hash(): Field {
    return Poseidon.hash(WrappingRequest.toFields(this));
  }
}

export class QueueWitness extends MerkleWitness(TREE_HEIGHT) {}

export class ZekoBridge extends SmartContract {
  @state(Field) treeRoot = State<Field>();

  @state(Field) firstIndex = State<Field>();
  @state(Field) lastIndex = State<Field>();

  init() {
    super.init();

    this.treeRoot.set(new MerkleTree(TREE_HEIGHT).getRoot());
    this.firstIndex.set(Field(0));
    this.lastIndex.set(Field(-1));
  }

  events = {
    'wrapping-request': WrappingRequest,
  };

  @method createWrappingRequest(
    request: WrappingRequest,
    witness: QueueWitness
  ) {
    // Create account update that sends funds from the sender to the zkapp
    const senderUpdate = AccountUpdate.createSigned(this.sender);
    senderUpdate.send({ to: this, amount: request.amount });

    // Public inputs
    const treeRoot = this.treeRoot.getAndAssertEquals();
    const lastIndex = this.lastIndex.getAndAssertEquals();

    // Assert we are not overflowing the list
    lastIndex.add(1).assertLessThan(new MerkleTree(TREE_HEIGHT).leafCount);

    // Assert correct new request index
    lastIndex.add(1).assertEquals(request.index);

    // Assert correct witness
    witness.calculateIndex().assertEquals(request.index);
    witness.calculateRoot(Field(0)).assertEquals(treeRoot);

    // Calculate new root
    const requestHash = request.hash();
    const newRoot = witness.calculateRoot(requestHash);

    this.treeRoot.set(newRoot);
    this.lastIndex.set(request.index);

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
    this.firstIndex.set(firstIndex.add(1));
  }
}
