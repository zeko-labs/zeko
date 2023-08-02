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
  QueueWitness,
  TREE_HEIGHT,
  WrappingRequest,
  ZekoBridge,
} from './ZekoBridge';
import config from './config';

const MINA = 1e9;

const buildMerkleTree = (requests: WrappingRequest[]): MerkleTree => {
  const tree = new MerkleTree(TREE_HEIGHT);

  requests.forEach((request) => {
    tree.setLeaf(request.index.toBigInt(), request.hash());
  });

  return tree;
};

describe('ZekoBridge', () => {
  let senderKey: PrivateKey;
  let senderAddress: PublicKey;

  let zkappKey: PrivateKey;
  let zkappAddress: PublicKey;
  let zkapp: ZekoBridge;

  let merkleTree: MerkleTree;
  let requests: WrappingRequest[];

  beforeAll(async () => {
    if (config.PROOFS_ENABLED) await ZekoBridge.compile();

    const Local = Mina.LocalBlockchain({
      proofsEnabled: config.PROOFS_ENABLED,
    });
    Mina.setActiveInstance(Local);

    ({ privateKey: senderKey, publicKey: senderAddress } =
      Local.testAccounts[0]);

    zkappKey = PrivateKey.random();
    zkappAddress = zkappKey.toPublicKey();
    zkapp = new ZekoBridge(zkappAddress);

    const deployTx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress, 1);
      zkapp.deploy({ zkappKey });
    });

    await deployTx.prove();
    await deployTx.sign([senderKey, zkappKey]).send();

    merkleTree = new MerkleTree(TREE_HEIGHT);
    requests = [];
  });

  it('deploys the `WrappingRequestList` zkapp', async () => {
    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.lastIndex.get()).toEqual(Field(-1));
  });

  it('creates first wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.lastIndex.get().add(1),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.lastIndex.get()).toEqual(Field(0));
  });

  it('creates second wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.lastIndex.get().add(1),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.lastIndex.get()).toEqual(Field(1));
  });

  it('creates third wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.lastIndex.get().add(1),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.lastIndex.get()).toEqual(Field(2));
  });

  it('pops first wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(1));
    expect(zkapp.lastIndex.get()).toEqual(Field(2));
  });

  it('pops second wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.lastIndex.get()).toEqual(Field(2));
  });

  it('creates fourth wrapping request', async () => {
    const request = new WrappingRequest({
      index: zkapp.lastIndex.get().add(1),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.lastIndex.get()).toEqual(Field(3));
  });

  it('pops third wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(3));
    expect(zkapp.lastIndex.get()).toEqual(Field(3));
  });

  it('pops fourth wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index.toBigInt()))
      );
    });

    merkleTree.setLeaf(request.index.toBigInt(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(buildMerkleTree(requests).getRoot()).toEqual(merkleTree.getRoot());

    expect(zkapp.firstIndex.get()).toEqual(Field(4));
    expect(zkapp.lastIndex.get()).toEqual(Field(3));
  });
});
