import {
  AccountUpdate,
  Field,
  Mina,
  PrivateKey,
  PublicKey,
  Reducer,
  TokenId,
  UInt64,
} from 'snarkyjs';
import { ExampleToken } from './ExampleToken';
import {
  ActionsRecursiveReducer,
  QueueWitness,
  ReducerState,
  WrappingRequest,
  WrappingRequestAction,
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

  let tokenKey: PrivateKey;
  let tokenAddress: PublicKey;
  let tokenZkapp: ExampleToken;

  let requests: WrappingRequest[];
  let pendingActions: WrappingRequestAction[];

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

    tokenKey = PrivateKey.random();
    tokenAddress = tokenKey.toPublicKey();
    tokenZkapp = new ExampleToken(tokenAddress);

    requests = [];
    pendingActions = [];

    await ActionsRecursiveReducer.compile();
  });

  it('deploys the `ZekoBridge` zkapp', async () => {
    const deployTx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress, 3);
      zkapp.deploy({ zkappKey });
      tokenZkapp.deploy({ zkappKey: tokenKey });

      tokenZkapp.mintTokens(senderAddress, UInt64.from(100 * MINA));
    });

    await deployTx.prove();
    await deployTx.sign([senderKey, zkappKey, tokenKey]).send();

    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );
    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(0));
    expect(zkapp.actionState.get()).toEqual(Reducer.initialActionState);
  });

  it('dispatches three wrapping requests', async () => {
    // Arrange
    const amount = UInt64.from(5 * MINA);
    const zkappBeforeBalance = Mina.getBalance(zkappAddress);

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createMinaWrappingRequest(pendingActions[0]);
      zkapp.createMinaWrappingRequest(pendingActions[1]);
      zkapp.createMinaWrappingRequest(pendingActions[2]);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp's balance has increased by the amount of the three
    // wrapping requests.
    expect(Mina.getBalance(zkappAddress)).toEqual(
      zkappBeforeBalance.add(amount.mul(3))
    );

    // Check that the zkapp's action state has been updated to the correct value.
    expect(zkapp.account.actionState.get()).toEqual(
      WrappingRequestAction.getActionState(
        pendingActions,
        Reducer.initialActionState
      )
    );

    // Check that the dispatched actions are same as local pending actions.
    expect(
      (
        await zkapp.reducer.fetchActions({
          fromActionState: zkapp.actionState.get(),
        })
      ).flat()
    ).toEqual(pendingActions);
  });

  it('rolls up actions using proof', async () => {
    // Arrange
    const proof = await WrappingRequestAction.buildReducerProof(
      pendingActions,
      new ReducerState({
        actionState: zkapp.actionState.get(),
        treeRoot: zkapp.treeRoot.get(),
        counter: zkapp.counter.get(),
      }),
      WrappingRequest.buildMerkleTree(requests)
    );

    pendingActions.reduce((counter, action) => {
      requests.push(
        new WrappingRequest({
          id: counter,
          actionState:
            requests.at(-1)?.getNextActionState() ?? Reducer.initialActionState,
          ...action,
        })
      );
      return counter.add(1);
    }, zkapp.counter.get());

    pendingActions = [];

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupRequestsWithProof(proof);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp state has been updated with the proof
    expect(zkapp.treeRoot.get()).toEqual(proof.publicInput.newState.treeRoot);
    expect(zkapp.counter.get()).toEqual(proof.publicInput.newState.counter);
    expect(zkapp.actionState.get()).toEqual(
      proof.publicInput.newState.actionState
    );

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(0));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('pops first wrapping request', async () => {
    // Arrange
    const request = requests[0];
    const witness = new QueueWitness(
      WrappingRequest.buildMerkleTree(requests).getWitness(request.index())
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(request.hash(), witness);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    requests.shift();

    // Assert

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(1));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('pops second wrapping request', async () => {
    // Arrange
    const request = requests[0];
    const witness = new QueueWitness(
      WrappingRequest.buildMerkleTree(requests).getWitness(request.index())
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(request.hash(), witness);
    });

    requests.shift();

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(Field(3));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });

  it('dispatches two wrapping requests', async () => {
    // Arrange
    const amount = UInt64.from(5 * MINA);
    const zkappBeforeBalance = Mina.getBalance(zkappAddress);

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createMinaWrappingRequest(pendingActions[0]);
      zkapp.createMinaWrappingRequest(pendingActions[1]);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp's balance has increased by the amount of the three
    // wrapping requests.
    expect(Mina.getBalance(zkappAddress)).toEqual(
      zkappBeforeBalance.add(amount.mul(2))
    );

    // Check that the zkapp's action state has been updated to the correct value.
    expect(zkapp.account.actionState.get()).toEqual(
      WrappingRequestAction.getActionState(
        pendingActions,
        zkapp.actionState.get()
      )
    );

    // Check that the dispatched actions are same as local pending actions.
    expect(
      (
        await zkapp.reducer.fetchActions({
          fromActionState: zkapp.actionState.get(),
        })
      ).flat()
    ).toEqual(pendingActions);
  });

  it('rolls up actions using proof and wraps the queue around the tree', async () => {
    // Arrange
    const proof = await WrappingRequestAction.buildReducerProof(
      pendingActions,
      new ReducerState({
        actionState: zkapp.actionState.get(),
        treeRoot: zkapp.treeRoot.get(),
        counter: zkapp.counter.get(),
      }),
      WrappingRequest.buildMerkleTree(requests)
    );

    pendingActions.reduce((counter, action) => {
      requests.push(
        new WrappingRequest({
          id: counter,
          actionState:
            requests.at(-1)?.getNextActionState() ?? Reducer.initialActionState,
          ...action,
        })
      );
      return counter.add(1);
    }, zkapp.counter.get());

    pendingActions = [];

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupRequestsWithProof(proof);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp state has been updated with the proof
    expect(zkapp.treeRoot.get()).toEqual(proof.publicInput.newState.treeRoot);
    expect(zkapp.counter.get()).toEqual(proof.publicInput.newState.counter);
    expect(zkapp.actionState.get()).toEqual(
      proof.publicInput.newState.actionState
    );

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(Field(5));

    expect(zkapp.lastIndex()).toEqual(UInt64.from(0));
  });

  it('dispatches one wrapping request', async () => {
    // Arrange
    const amount = UInt64.from(5 * MINA);
    const zkappBeforeBalance = Mina.getBalance(zkappAddress);

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: TokenId.default,
        receiver: senderAddress,
      })
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.createMinaWrappingRequest(pendingActions[0]);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp's balance has increased by the amount of the three
    // wrapping requests.
    expect(Mina.getBalance(zkappAddress)).toEqual(
      zkappBeforeBalance.add(amount)
    );

    // Check that the zkapp's action state has been updated to the correct value.
    expect(zkapp.account.actionState.get()).toEqual(
      WrappingRequestAction.getActionState(
        pendingActions,
        zkapp.actionState.get()
      )
    );

    // Check that the dispatched actions are same as local pending actions.
    expect(
      (
        await zkapp.reducer.fetchActions({
          fromActionState: zkapp.actionState.get(),
        })
      ).flat()
    ).toEqual(pendingActions);
  });

  it('rolls up actions without the proof', async () => {
    // Arrange
    const { batch, newState } = WrappingRequestAction.buildBatch(
      pendingActions,
      new ReducerState({
        actionState: zkapp.actionState.get(),
        treeRoot: zkapp.treeRoot.get(),
        counter: zkapp.counter.get(),
      }),
      WrappingRequest.buildMerkleTree(requests)
    );

    pendingActions.reduce((counter, action) => {
      requests.push(
        new WrappingRequest({
          id: counter,
          actionState:
            requests.at(-1)?.getNextActionState() ?? Reducer.initialActionState,
          ...action,
        })
      );
      return counter.add(1);
    }, zkapp.counter.get());

    pendingActions = [];

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupRequests(batch);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp state has been updated with the proof
    expect(zkapp.treeRoot.get()).toEqual(newState.treeRoot);
    expect(zkapp.counter.get()).toEqual(newState.counter);
    expect(zkapp.actionState.get()).toEqual(newState.actionState);

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(2));
    expect(zkapp.counter.get()).toEqual(Field(6));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(1));
  });

  it('pops third wrapping request', async () => {
    // Arrange
    const request = requests[0];
    const witness = new QueueWitness(
      WrappingRequest.buildMerkleTree(requests).getWitness(request.index())
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.popWrappingRequest(request.hash(), witness);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    requests.shift();

    // Assert

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(3));
    expect(zkapp.counter.get()).toEqual(Field(6));
    expect(zkapp.lastIndex()).toEqual(UInt64.from(1));
  });

  it('dispatches token request', async () => {
    // Arrange
    const amount = UInt64.from(5 * MINA);
    const senderBeforeBalance = Mina.getBalance(
      senderAddress,
      tokenZkapp.token.id
    );

    pendingActions.push(
      new WrappingRequestAction({
        amount,
        tokenId: tokenZkapp.token.id,
        receiver: senderAddress,
      })
    );

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      AccountUpdate.fundNewAccount(senderAddress);
      const accUpdate = tokenZkapp.sendTokens(
        senderAddress,
        zkappAddress,
        amount
      );
      zkapp.createTokenWrappingRequest(pendingActions[0], accUpdate);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey, tokenKey]).send();

    // Assert

    // Check that the zkapp's balance has increased by the amount of the three
    // wrapping requests.
    expect(Mina.getBalance(zkappAddress, tokenZkapp.token.id)).toEqual(amount);
    expect(Mina.getBalance(senderAddress, tokenZkapp.token.id)).toEqual(
      senderBeforeBalance.sub(amount)
    );

    // Check that the zkapp's action state has been updated to the correct value.
    expect(zkapp.account.actionState.get()).toEqual(
      WrappingRequestAction.getActionState(
        pendingActions,
        zkapp.actionState.get()
      )
    );

    // Check that the dispatched actions are same as local pending actions.
    expect(
      (
        await zkapp.reducer.fetchActions({
          fromActionState: zkapp.actionState.get(),
        })
      ).flat()
    ).toEqual(pendingActions);
  });

  it('rolls up token request', async () => {
    // Arrange
    const { batch, newState } = WrappingRequestAction.buildBatch(
      pendingActions,
      new ReducerState({
        actionState: zkapp.actionState.get(),
        treeRoot: zkapp.treeRoot.get(),
        counter: zkapp.counter.get(),
      }),
      WrappingRequest.buildMerkleTree(requests)
    );

    pendingActions.reduce((counter, action) => {
      requests.push(
        new WrappingRequest({
          id: counter,
          actionState:
            requests.at(-1)?.getNextActionState() ?? Reducer.initialActionState,
          ...action,
        })
      );
      return counter.add(1);
    }, zkapp.counter.get());

    pendingActions = [];

    // Act
    const tx = await Mina.transaction(senderAddress, () => {
      zkapp.rollupRequests(batch);
    });

    await tx.prove();
    await tx.sign([senderKey, zkappKey]).send();

    // Assert

    // Check that the zkapp state has been updated with the proof
    expect(zkapp.treeRoot.get()).toEqual(newState.treeRoot);
    expect(zkapp.counter.get()).toEqual(newState.counter);
    expect(zkapp.actionState.get()).toEqual(newState.actionState);

    // Check that the tree root is correct
    expect(zkapp.treeRoot.get()).toEqual(
      WrappingRequest.buildMerkleTree(requests).getRoot()
    );

    // Check that indices are correct
    expect(zkapp.firstIndex.get()).toEqual(Field(3));
    expect(zkapp.counter.get()).toEqual(Field(7));

    expect(zkapp.lastIndex()).toEqual(UInt64.from(2));
  });
});
