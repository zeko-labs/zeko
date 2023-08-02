import {
  AccountUpdate,
  Field,
  MerkleTree,
  Mina,
  PrivateKey,
  PublicKey,
  UInt64,
} from 'snarkyjs';
import {
  ListWitness,
  MerkleTreeArray,
  TREE_HEIGHT,
  WrappingRequest,
} from './MerkleTreeArray';
import config from './config';

const MINA = 1e9;

describe('WrappingRequestList', () => {
  let senderKey: PrivateKey;
  let senderAddress: PublicKey;

  let zkappKey: PrivateKey;
  let zkappAddress: PublicKey;
  let zkapp: MerkleTreeArray;

  let merkleTree: MerkleTree;
  let list: WrappingRequest[];

  beforeAll(async () => {
    if (config.PROOFS_ENABLED) await MerkleTreeArray.compile();

    const Local = Mina.LocalBlockchain({
      proofsEnabled: config.PROOFS_ENABLED,
    });
    Mina.setActiveInstance(Local);

    ({ privateKey: senderKey, publicKey: senderAddress } =
      Local.testAccounts[0]);

    zkappKey = PrivateKey.random();
    zkappAddress = zkappKey.toPublicKey();
    zkapp = new MerkleTreeArray(zkappAddress);

    const deployTx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress, 1);
      zkapp.deploy({ zkappKey });
    });

    await deployTx.prove();
    await deployTx.sign([senderKey, zkappKey]).send();

    merkleTree = new MerkleTree(TREE_HEIGHT);
    list = [];
  });

  it('deploys the `WrappingRequestList` zkapp', async () => {
    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(UInt64.zero);
  });

  it('creates first wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new ListWitness(merkleTree.getWitness(0n))
      );
    });

    merkleTree.setLeaf(0n, request.hash());
    list.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(1));
    expect(zkapp.counter.get()).toEqual(UInt64.one);

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expectedMerkleTree.fill(list.map((r) => r.hash()));
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });

  it('creates second wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.counter.get(),
      amount: UInt64.from(10 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new ListWitness(merkleTree.getWitness(1n))
      );
    });

    merkleTree.setLeaf(1n, request.hash());
    list.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(UInt64.from(2));

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expectedMerkleTree.fill(list.map((r) => r.hash()));
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });

  it('creates third wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.counter.get(),
      amount: UInt64.from(10 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new ListWitness(merkleTree.getWitness(2n))
      );
    });

    merkleTree.setLeaf(2n, request.hash());
    list.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(3));
    expect(zkapp.counter.get()).toEqual(UInt64.from(3));

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expectedMerkleTree.fill(list.map((r) => r.hash()));
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });

  it('removes second wrapping request', async () => {
    const indexToRemove = 1n;

    const firstWitness = new ListWitness(merkleTree.getWitness(indexToRemove));
    merkleTree.setLeaf(indexToRemove, list.at(-1)?.hash() ?? Field(0));

    const secondWitness = new ListWitness(
      merkleTree.getWitness(BigInt(list.length - 1))
    );
    merkleTree.setLeaf(BigInt(list.length - 1), Field(0));

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.removeWrappingRequest(
        firstWitness,
        list[+indexToRemove.toString()].hash(),
        secondWitness,
        list[list.length - 1].hash()
      );
    });

    list.splice(+indexToRemove.toString(), 1);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(UInt64.from(3));

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expectedMerkleTree.fill(list.map((r) => r.hash()));
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });

  it('removes last (third) wrapping request', async () => {
    const indexToRemove = 1n;

    const witness = new ListWitness(merkleTree.getWitness(indexToRemove));
    merkleTree.setLeaf(indexToRemove, Field(0));

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.removeWrappingRequest(
        witness,
        list[+indexToRemove.toString()].hash(),
        witness,
        list[+indexToRemove.toString()].hash()
      );
    });

    list.splice(+indexToRemove.toString(), 1);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(1));
    expect(zkapp.counter.get()).toEqual(UInt64.from(3));

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expectedMerkleTree.fill(list.map((r) => r.hash()));
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });

  it('removes last (first) wrapping request', async () => {
    const indexToRemove = 0n;

    const witness = new ListWitness(merkleTree.getWitness(indexToRemove));
    merkleTree.setLeaf(indexToRemove, Field(0));

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.removeWrappingRequest(
        witness,
        list[+indexToRemove.toString()].hash(),
        witness,
        list[+indexToRemove.toString()].hash()
      );
    });

    list = [];

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.listRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.listSize.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(UInt64.from(3));

    const expectedMerkleTree = new MerkleTree(TREE_HEIGHT);
    expect(expectedMerkleTree.getRoot()).toEqual(merkleTree.getRoot());
  });
});
