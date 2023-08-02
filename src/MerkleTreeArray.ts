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

export const TREE_HEIGHT = 32;

export class WrappingRequest extends Struct({
  index: UInt64,
  amount: UInt64,
  receiver: PublicKey,
}) {
  hash(): Field {
    return Poseidon.hash(WrappingRequest.toFields(this));
  }
}

export class ListWitness extends MerkleWitness(TREE_HEIGHT) {}

export class MerkleTreeArray extends SmartContract {
  @state(Field) listRoot = State<Field>();
  @state(Field) listSize = State<Field>();

  @state(UInt64) counter = State<UInt64>();

  init() {
    super.init();

    this.listRoot.set(new MerkleTree(TREE_HEIGHT).getRoot());
    this.listSize.set(Field(0));
    this.counter.set(UInt64.zero);
  }

  events = {
    'wrapping-request': WrappingRequest,
  };

  @method createWrappingRequest(
    request: WrappingRequest,
    witness: ListWitness
  ) {
    // Create account update that sends funds from the sender to the zkapp
    const senderUpdate = AccountUpdate.createSigned(this.sender);
    senderUpdate.send({ to: this, amount: request.amount });

    // Public inputs
    const listRoot = this.listRoot.getAndAssertEquals();
    const listSize = this.listSize.getAndAssertEquals();
    const counter = this.counter.getAndAssertEquals();

    // Assert we are not overflowing the list
    listSize.assertLessThan(new MerkleTree(TREE_HEIGHT).leafCount);

    // Assert correct new request index
    counter.assertEquals(request.index);
    this.counter.set(counter.add(1));

    // Assert correct witness
    witness.calculateRoot(Field(0)).assertEquals(listRoot);
    witness.calculateIndex().assertEquals(listSize);

    // Update list root and size
    const newQueueRoot = witness.calculateRoot(request.hash());

    this.listRoot.set(newQueueRoot);
    this.listSize.set(listSize.add(1));

    this.emitEvent('wrapping-request', request);
  }

  @method removeWrappingRequest(
    requestWitness: ListWitness,
    requestLeaf: Field,
    // This needs to be calculated after first update
    // If it's the last request, it's the same as request to remove
    lastRequestWitness: ListWitness,
    lastRequestLeaf: Field
  ) {
    /**
     * some logic to check if the request can be removed
     */

    const listRoot = this.listRoot.getAndAssertEquals();
    const listSize = this.listSize.getAndAssertEquals();

    const isThisLastRequest = listSize
      .sub(1)
      .equals(requestWitness.calculateIndex());

    // Assert correct witness for the request to be removed
    requestWitness.calculateRoot(requestLeaf).assertEquals(listRoot);

    // Copy last request to the removed request position
    const firstUpdateRoot = requestWitness.calculateRoot(lastRequestLeaf);

    // Assert correct witness for the last request after first update
    lastRequestWitness.calculateIndex().assertEquals(listSize.sub(1));
    lastRequestWitness
      .calculateRoot(lastRequestLeaf)
      .assertEquals(Provable.if(isThisLastRequest, listRoot, firstUpdateRoot));

    // Remove last request
    const newRoot = lastRequestWitness.calculateRoot(Field(0));

    this.listRoot.set(newRoot);
    this.listSize.set(listSize.sub(1));
  }
}
