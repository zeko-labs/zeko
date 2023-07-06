import { ethers } from 'ethers';
import {
  Base58Encodings,
  Field,
  Group,
  Ledger,
  Mina,
  PublicKey,
  Scalar,
  Signature,
} from 'snarkyjs';
import { daLayerContract } from './daLayer';
import {
  Account,
  SendPaymentInput,
  ZkappCommandInput,
} from './generated/graphql';
import { MinaCommandStruct } from './typechain-types/contracts/DataAvailability';
import { convAuthRequiredToGqlType } from './utils';

export type GenesisAccount = {
  publicKey: PublicKey;
  balance: number | string;
};

export type RollupContext = {
  rollup: Rollup;
};

export enum CommandType {
  Payment = 0,
  Zkapp = 1,
}

export type Transaction = {
  id: string;
  commandType: CommandType;
};

export type Batch = {
  batchId: string;
  transactions: Transaction[];
};

export type ZkappEvent = {
  data: string[];
  txHash: string;
  txStatus: string;
  txMemo: string;
  authorizationKind: string;
};

export type ZkappAction = {
  data: string[];
  state: string[];
  accountUpdateId: number;
  txHash: string;
  txStatus: string;
  txMemo: string;
  authorizationKind: string;
};

export class Rollup {
  public ledger: Ledger;

  public readonly networkConstants;
  public networkState;

  public stagedTransactions: Transaction[] = [];
  public batches: Batch[] = [];

  public events: Map<string, ZkappEvent[]> = new Map();
  public actions: Map<string, ZkappAction[]> = new Map();

  public accountUpdateCounter = 0;

  constructor(
    genesisAccounts: GenesisAccount[],
    accountCreationFee: number | string
  ) {
    this.ledger = Ledger.create(
      genesisAccounts.map(({ publicKey, balance }) => ({
        publicKey,
        balance: balance.toString(),
      }))
    );

    const dummyBlockchain = Mina.LocalBlockchain({
      accountCreationFee: accountCreationFee.toString(),
      proofsEnabled: true,
    });

    this.networkState = dummyBlockchain.getNetworkState();
    this.networkConstants = dummyBlockchain.getNetworkConstants();
  }

  getAccount(publicKey: PublicKey, token: Field): Account | null {
    const acc = this.ledger.getAccount(publicKey, token);

    if (acc === undefined) return null;

    return {
      publicKey: acc.publicKey,
      balance: {
        blockHeight: this.networkState.blockchainLength,
        liquid: acc.balance,
        locked: '0',
        stateHash: this.networkState.snarkedLedgerHash,
        total: acc.balance,
        unknown: '0',
      },
      nonce: acc.nonce,
      inferredNonce: acc.nonce,
      permissions: {
        access: convAuthRequiredToGqlType(acc.permissions.access),
        editActionState: convAuthRequiredToGqlType(
          acc.permissions.editActionState
        ),
        editState: convAuthRequiredToGqlType(acc.permissions.editState),
        incrementNonce: convAuthRequiredToGqlType(
          acc.permissions.incrementNonce
        ),
        receive: convAuthRequiredToGqlType(acc.permissions.receive),
        send: convAuthRequiredToGqlType(acc.permissions.send),
        setDelegate: convAuthRequiredToGqlType(acc.permissions.setDelegate),
        setPermissions: convAuthRequiredToGqlType(
          acc.permissions.setPermissions
        ),
        setTiming: convAuthRequiredToGqlType(acc.permissions.setTiming),
        setTokenSymbol: convAuthRequiredToGqlType(
          acc.permissions.setTokenSymbol
        ),
        setVerificationKey: convAuthRequiredToGqlType(
          acc.permissions.setVerificationKey
        ),
        setVotingFor: convAuthRequiredToGqlType(acc.permissions.setVotingFor),
        setZkappUri: convAuthRequiredToGqlType(acc.permissions.setZkappUri),
      },
      provedState: false, // TODO
      receiptChainHash: Base58Encodings.ReceiptChainHash.toBase58(
        Field(acc.receiptChainHash)
      ),
      timing: acc.timing,

      tokenId: acc.tokenId,
      token: acc.tokenId,
      tokenSymbol: acc.tokenSymbol,

      verificationKey: acc.zkapp?.verificationKey && {
        hash: acc.zkapp?.verificationKey?.hash,
        verificationKey: acc.zkapp?.verificationKey?.data,
      },
      zkappState: acc.zkapp?.appState,
      zkappUri: acc.zkapp?.zkappUri,
      actionState: acc.zkapp?.actionState,

      // we don't support delegation
      delegate: null,
      delegateAccount: null,
      delegators: null,
      lastEpochDelegators: null,
      stakingActive: false,
      votingFor: null,

      // TODO
      index: null,
      leafHash: null,
      locked: false,
      merklePath: null,
      privateKeyPath: '',
    };
  }

  storeActions(
    publicKey: string,
    tokenId: string,
    txHash: string,
    txMemo: string,
    actions: string[][],
    authorizationKind: string
  ) {
    if (actions.length === 0) return;

    const zkappAccount = this.ledger.getAccount(
      PublicKey.fromBase58(publicKey),
      Base58Encodings.TokenId.fromBase58(tokenId)
    );

    if (zkappAccount === undefined || zkappAccount.zkapp === null) return;

    const previousActions = this.actions.get(`${publicKey}-${tokenId}`) ?? [];

    this.actions.set(`${publicKey}-${tokenId}`, [
      ...actions.map((action) => ({
        data: action,
        // @ts-expect-error we checked if zkapp is null, typescript is just dumb
        state: zkappAccount.zkapp.actionState,
        accountUpdateId: this.accountUpdateCounter,
        txHash,
        txStatus: 'applied',
        txMemo,
        authorizationKind,
      })),
      ...previousActions,
    ]);
  }

  storeEvents(
    publicKey: string,
    tokenId: string,
    txHash: string,
    txMemo: string,
    events: string[][],
    authorizationKind: string
  ) {
    if (events.length === 0) return;

    const previousEvents = this.events.get(`${publicKey}-${tokenId}`) ?? [];

    this.events.set(`${publicKey}-${tokenId}`, [
      ...events.map((event) => ({
        data: event,
        txHash,
        txStatus: 'applied',
        txMemo,
        authorizationKind,
      })),
      ...previousEvents,
    ]);
  }

  updateNetworkState() {
    this.networkState.blockchainLength =
      this.networkState.blockchainLength.add(1);
    this.networkState.globalSlotSinceGenesis =
      this.networkState.globalSlotSinceGenesis.add(1);
  }

  applyZkappCommand(zkappCommand: ZkappCommandInput): {
    hash: string;
    id: string;
  } {
    const result = this.ledger.applyJsonTransaction(
      JSON.stringify(zkappCommand),
      this.networkConstants.accountCreationFee.toString(),
      JSON.stringify(this.networkState)
    );

    zkappCommand.accountUpdates.forEach((accountUpdate) => {
      this.accountUpdateCounter++;

      const {
        publicKey,
        tokenId,
        events,
        actions,
        authorizationKind,
      }: {
        publicKey: string;
        tokenId: string;
        events: string[][];
        actions: string[][];
        authorizationKind: {
          isProved: boolean;
          isSigned: boolean;
        };
      } = accountUpdate.body;

      let authorizationKindString: string;

      if (authorizationKind.isProved && authorizationKind.isSigned)
        authorizationKindString = 'Either';
      else if (authorizationKind.isSigned)
        authorizationKindString = 'Signature';
      else if (authorizationKind.isProved) authorizationKindString = 'Proof';
      else authorizationKindString = 'None';

      this.storeActions(
        publicKey,
        tokenId,
        result.hash,
        zkappCommand.memo,
        actions,
        authorizationKindString
      );
      this.storeEvents(
        publicKey,
        tokenId,
        result.hash,
        zkappCommand.memo,
        events,
        authorizationKindString
      );
    });

    this.updateNetworkState();

    this.stagedTransactions.push({
      ...result,
      commandType: CommandType.Zkapp,
    });

    return result;
  }

  applyPayment(
    signature: Signature,
    userCommand: SendPaymentInput
  ): { hash: string; id: string } {
    const { from, to, amount, fee, validUntil, nonce, memo } = userCommand;

    const result = this.ledger.applyPayment(
      signature.toBase58(),
      from,
      to,
      amount.toString(),
      fee.toString(),
      validUntil.toString(),
      nonce.toString(),
      Ledger.memoToBase58(memo?.toString() ?? ''),
      this.networkConstants.accountCreationFee.toString(),
      JSON.stringify(this.networkState)
    );

    this.updateNetworkState();

    this.stagedTransactions.push({
      ...result,
      commandType: CommandType.Payment,
    });

    return result;
  }

  applyBatch(batch: Batch): void {
    batch.transactions.forEach((tx) => {
      switch (tx.commandType) {
        case CommandType.Payment:
          this.ledger.applyBase64Payment(
            tx.id,
            this.networkConstants.accountCreationFee.toString(),
            JSON.stringify(this.networkState)
          );

          break;
        case CommandType.Zkapp:
          this.ledger.applyJsonTransaction(
            Ledger.encoding.jsonZkappCommandFromBase64(tx.id),
            this.networkConstants.accountCreationFee.toString(),
            JSON.stringify(this.networkState)
          );
          break;
      }
    });

    this.batches.push(batch);
  }

  async bootstrap(lastBatchId: string): Promise<void> {
    console.log('Bootstrapping the branch', lastBatchId);

    let currentBatchId = lastBatchId;
    let previousBatchId;
    let transactions;

    const reversedBatches: Batch[] = [];

    while (currentBatchId !== ethers.constants.HashZero) {
      [previousBatchId, transactions] = await daLayerContract.getBatchData(
        currentBatchId
      );

      const batch = {
        batchId: currentBatchId,
        transactions: transactions.map((tx) => ({
          id: Buffer.from(tx.data.slice(2), 'hex').toString('base64'),
          commandType: tx.commandType,
        })),
      };

      reversedBatches.push(batch);

      currentBatchId = previousBatchId;
    }

    reversedBatches.reverse().forEach((batch) => {
      this.applyBatch(batch);
      console.log('Applied batch: ', batch.batchId);
    });
  }

  async commit(): Promise<void> {
    const stagedTransactions = this.stagedTransactions.slice();
    this.stagedTransactions = [];

    const previousBatchId =
      this.batches.at(-1)?.batchId ?? ethers.constants.HashZero;

    const proposedCommands: MinaCommandStruct[] = stagedTransactions.map(
      ({ id, commandType }) => ({
        commandType,
        data: Buffer.from(id, 'base64'),
      })
    );

    const tx = await daLayerContract.proposeBatch(
      previousBatchId,
      proposedCommands
    );

    const receipt = await tx.wait();

    const proposedBatchId = receipt.events?.find(
      (e) => e.event === 'BatchProposed'
    )?.args?.batchId;

    console.log('Batch posted: ', proposedBatchId);

    this.batches.push({
      batchId: proposedBatchId,
      transactions: stagedTransactions,
    });

    const quorum = await daLayerContract.quorum();

    const batchFields = (
      await daLayerContract.getBatchFields(proposedBatchId)
    ).map((hexField) =>
      Field.fromBytes(Array.from(Buffer.from(hexField.slice(2), 'hex')))
    );

    daLayerContract.on('BatchSigned', async (batchId, _, signatureCount) => {
      if (signatureCount < quorum || proposedBatchId !== batchId) return;

      const solSignatures = await daLayerContract.getBatchSignatures(batchId);

      const signatures = solSignatures.map((sig) => {
        const pubKeyGroup = Group.fromJSON({
          x: Field.fromBytes(
            Array.from(Buffer.from(sig.publicKey.x.slice(2), 'hex'))
          ).toString(),
          y: Field.fromBytes(
            Array.from(Buffer.from(sig.publicKey.y.slice(2), 'hex'))
          ).toString(),
        });

        if (pubKeyGroup === null) throw new Error('Invalid public key');

        const publicKey = PublicKey.fromGroup(pubKeyGroup);

        const signature = Signature.fromJSON({
          r: Field.fromBytes(
            Array.from(Buffer.from(sig.rx.slice(2), 'hex'))
          ).toString(),
          s: Scalar.fromJSON(
            Field.fromBytes(
              Array.from(Buffer.from(sig.s.slice(2), 'hex'))
            ).toString()
          ),
        });

        console.log(signature.verify(publicKey, batchFields).toBoolean());

        return {
          publicKey,
          signature,
        };
      });

      console.log('Batch committed: ', batchId, signatures.length);
    });
  }
}