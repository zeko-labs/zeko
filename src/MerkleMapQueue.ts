import {
  AccountUpdate,
  Field,
  MerkleMap,
  MerkleMapWitness,
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

export const EMPTY_LEAF_VALUE = Field(0);
export const LAST_ELEMENT_LEAF_VALUE = Field(69);

export class WrappingRequest extends Struct({
  index: UInt64,
  amount: UInt64,
  receiver: PublicKey,
}) {
  hash(): Field {
    return Poseidon.hash(WrappingRequest.toFields(this));
  }
}

export class MerkleMapQueue extends SmartContract {
  @state(Field) queueRoot = State<Field>();
  @state(UInt64) counter = State<UInt64>();

  @state(Field) firstRequest = State<Field>();
  @state(Field) lastRequest = State<Field>();

  init() {
    super.init();

    this.queueRoot.set(new MerkleMap().getRoot());
    this.counter.set(UInt64.zero);

    this.firstRequest.set(Field(0));
    this.lastRequest.set(Field(0));
  }

  events = {
    'wrapping-request': WrappingRequest,
  };

  @method createWrappingRequest(
    newRequest: WrappingRequest,
    newRequestWitness: MerkleMapWitness,
    // This needs to be calculated after newRequest update
    // If it's the first request, it's the same as newRequestWitness
    lastRequestWitness: MerkleMapWitness
  ) {
    // Public inputs
    const counter = this.counter.getAndAssertEquals();
    const storedQueueRoot = this.queueRoot.getAndAssertEquals();
    const storedFirstRequestKey = this.firstRequest.getAndAssertEquals();
    const storedLastRequestKey = this.lastRequest.getAndAssertEquals();

    const isThisFirstRequest = storedFirstRequestKey.equals(Field(0));

    // Create account update that sends funds from the sender to the zkapp
    const senderUpdate = AccountUpdate.createSigned(this.sender);
    senderUpdate.send({ to: this, amount: newRequest.amount });

    // Assert correct new request index
    counter.assertEquals(newRequest.index);
    this.counter.set(counter.add(1));

    // Assert correct and empty new request witness
    const [newRequestWitnessRoot, newRequestWitnessKey] =
      newRequestWitness.computeRootAndKey(EMPTY_LEAF_VALUE);

    const newRequestKey = newRequest.hash();
    newRequestWitnessKey.assertEquals(newRequestKey);
    newRequestWitnessRoot.assertEquals(storedQueueRoot);

    // Update map with new request
    const [firstUpdateRoot] = newRequestWitness.computeRootAndKey(
      LAST_ELEMENT_LEAF_VALUE
    );

    // Assert correct last request witness after first update
    // If it's the first one, it's the same as newRequest
    const [lastRequestWitnessRoot, lastRequestWitnessKey] =
      lastRequestWitness.computeRootAndKey(
        Provable.if(
          isThisFirstRequest,
          EMPTY_LEAF_VALUE,
          LAST_ELEMENT_LEAF_VALUE
        )
      );

    lastRequestWitnessKey.assertEquals(
      Provable.if(isThisFirstRequest, newRequestKey, storedLastRequestKey)
    );
    lastRequestWitnessRoot.assertEquals(
      Provable.if(isThisFirstRequest, storedQueueRoot, firstUpdateRoot)
    );

    // Update last request
    const [newRoot] = lastRequestWitness.computeRootAndKey(newRequestKey);

    this.queueRoot.set(
      Provable.if(isThisFirstRequest, firstUpdateRoot, newRoot)
    );
    this.lastRequest.set(newRequestKey);
    this.firstRequest.set(
      Provable.if(isThisFirstRequest, newRequestKey, storedFirstRequestKey)
    );

    this.emitEvent('wrapping-request', newRequest);
  }

  @method popWrappingRequest(
    firstRequestWitness: MerkleMapWitness,
    // If it's the last request, it's `LAST_ELEMENT_LEAF_VALUE`
    secondRequestKey: Field
  ) {
    /**
     * some logic to check if the request can be popped
     */

    // Public inputs
    const storedQueueRoot = this.queueRoot.getAndAssertEquals();
    const storedFirstRequestKey = this.firstRequest.getAndAssertEquals();
    const storedLastRequestKey = this.lastRequest.getAndAssertEquals();

    const isThisLastRequest =
      storedFirstRequestKey.equals(storedLastRequestKey);

    // Assert correct first request witness
    const [firstRequestWitnessRoot, firstRequestWitnessKey] =
      firstRequestWitness.computeRootAndKey(secondRequestKey);

    firstRequestWitnessRoot.assertEquals(storedQueueRoot);
    firstRequestWitnessKey.assertEquals(storedFirstRequestKey);

    // Update first request
    const [newRoot] = firstRequestWitness.computeRootAndKey(EMPTY_LEAF_VALUE);

    this.queueRoot.set(newRoot);
    this.firstRequest.set(
      Provable.if(isThisLastRequest, Field(0), secondRequestKey)
    );
    this.lastRequest.set(
      Provable.if(isThisLastRequest, Field(0), storedLastRequestKey)
    );
  }
}
