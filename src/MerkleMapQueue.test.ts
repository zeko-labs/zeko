import {
  AccountUpdate,
  Field,
  MerkleMap,
  Mina,
  PrivateKey,
  PublicKey,
  UInt64,
} from 'snarkyjs';
import {
  EMPTY_LEAF_VALUE,
  LAST_ELEMENT_LEAF_VALUE,
  MerkleMapQueue,
  WrappingRequest,
} from './MerkleMapQueue';
import config from './config';

const MINA = 1e9;

describe('ZekoBridge', () => {
  let senderKey: PrivateKey;
  let senderAddress: PublicKey;

  let zkappKey: PrivateKey;
  let zkappAddress: PublicKey;
  let zkapp: MerkleMapQueue;

  let merkleMap: MerkleMap;
  let queue: WrappingRequest[];

  beforeAll(async () => {
    if (config.PROOFS_ENABLED) await MerkleMapQueue.compile();

    const Local = Mina.LocalBlockchain({
      proofsEnabled: config.PROOFS_ENABLED,
    });
    Mina.setActiveInstance(Local);

    ({ privateKey: senderKey, publicKey: senderAddress } =
      Local.testAccounts[0]);

    zkappKey = PrivateKey.random();
    zkappAddress = zkappKey.toPublicKey();
    zkapp = new MerkleMapQueue(zkappAddress);

    const deployTx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress, 1);
      zkapp.deploy({ zkappKey });
    });

    await deployTx.prove();
    await deployTx.sign([senderKey, zkappKey]).send();

    merkleMap = new MerkleMap();
    queue = [];
  });

  it('deploys the `WrappingRequestQueue` zkapp', async () => {
    expect(zkapp.queueRoot.get()).toEqual(merkleMap.getRoot());
    expect(zkapp.counter.get()).toEqual(UInt64.zero);

    expect(zkapp.firstRequest.get()).toEqual(Field(0));
    expect(zkapp.lastRequest.get()).toEqual(Field(0));
  });

  it('creates first wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.counter.get(),
      amount: UInt64.from(10 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        merkleMap.getWitness(request.hash()),
        merkleMap.getWitness(request.hash())
      );
    });

    merkleMap.set(request.hash(), LAST_ELEMENT_LEAF_VALUE);
    queue.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.queueRoot.get()).toEqual(merkleMap.getRoot());
    expect(zkapp.counter.get()).toEqual(UInt64.one);

    expect(zkapp.firstRequest.get()).toEqual(request.hash());
    expect(zkapp.lastRequest.get()).toEqual(request.hash());
  });

  it('creates second wrapping request', async () => {
    const preTxFirstRequest = zkapp.firstRequest.get();

    const request = new WrappingRequest({
      index: zkapp.counter.get(),
      amount: UInt64.from(10 * MINA),
      receiver: senderAddress,
    });

    const newRequestWitness = merkleMap.getWitness(request.hash());
    merkleMap.set(request.hash(), LAST_ELEMENT_LEAF_VALUE);

    const lastRequestWitness = merkleMap.getWitness(zkapp.lastRequest.get());
    merkleMap.set(zkapp.lastRequest.get(), request.hash());

    queue.push(request);

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        newRequestWitness,
        lastRequestWitness
      );
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.queueRoot.get()).toEqual(merkleMap.getRoot());
    expect(zkapp.counter.get()).toEqual(UInt64.from(2));

    expect(zkapp.firstRequest.get()).toEqual(preTxFirstRequest);
    expect(zkapp.lastRequest.get()).toEqual(request.hash());
  });

  it('pops the first wrapping request', async () => {
    const preTxLastRequest = zkapp.lastRequest.get();

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        merkleMap.getWitness(queue[0].hash()),
        queue[1].hash()
      );
    });

    merkleMap.set(queue[0].hash(), EMPTY_LEAF_VALUE);
    queue.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.queueRoot.get()).toEqual(merkleMap.getRoot());
    expect(zkapp.counter.get()).toEqual(UInt64.from(2));

    expect(zkapp.firstRequest.get()).toEqual(queue[0].hash());
    expect(zkapp.lastRequest.get()).toEqual(preTxLastRequest);
  });

  it('pops the second wrapping request', async () => {
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        merkleMap.getWitness(queue[0].hash()),
        LAST_ELEMENT_LEAF_VALUE
      );
    });

    merkleMap.set(queue[0].hash(), EMPTY_LEAF_VALUE);
    queue.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.queueRoot.get()).toEqual(merkleMap.getRoot());
    expect(zkapp.counter.get()).toEqual(UInt64.from(2));

    expect(zkapp.firstRequest.get()).toEqual(Field(0));
    expect(zkapp.lastRequest.get()).toEqual(Field(0));
  });
});
