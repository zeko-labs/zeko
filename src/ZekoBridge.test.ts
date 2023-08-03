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
    expect(zkapp.counter.get()).toEqual(Field(0));
  });

  it('creates first wrapping request', async () => {
    const request = new WrappingRequest({
      id: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(1));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(0));
  });

  it('creates second wrapping request', async () => {
    const request = new WrappingRequest({
      id: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(2));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(1));
  });

  it('creates third wrapping request', async () => {
    const request = new WrappingRequest({
      id: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('pops first wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(1));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('pops second wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('creates fourth wrapping request', async () => {
    const request = new WrappingRequest({
      id: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(Field(4));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(3));
  });

  it('pops third wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(3));
    expect(zkapp.counter.get()).toEqual(Field(4));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(3));
  });

  it('creates fifth wrapping request and wraps the queue around the tree', async () => {
    const request = new WrappingRequest({
      id: zkapp.counter.get(),
      amount: UInt64.from(5 * MINA),
      receiver: senderAddress,
    });

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createWrappingRequest(
        request,
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), request.hash());
    requests.push(request);

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(3));
    expect(zkapp.counter.get()).toEqual(Field(5));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(0));
  });

  it('pops fourth wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(5));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(0));
  });

  it('pops fifth wrapping request', async () => {
    const request = requests[0];

    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(
        request.hash(),
        new QueueWitness(merkleTree.getWitness(request.index()))
      );
    });

    merkleTree.setLeaf(request.index(), Field(0));
    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(merkleTree.getRoot());
    expect(WrappingRequest.buildMerkleTree(requests).getRoot()).toEqual(
      merkleTree.getRoot()
    );

    expect(zkapp.firstIndex.get()).toEqual(Field(1));
    expect(zkapp.counter.get()).toEqual(Field(5));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(0));
  });
});
