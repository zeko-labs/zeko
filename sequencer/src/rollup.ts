import { ethers } from "ethers";
import { Base58Encodings, Field, FieldConst, Ledger, Mina, MlPublicKey, Signature, Test } from "snarkyjs";
import { fetchBatches, postBatch } from "./daLayer";
import { Account, SendPaymentInput, ZkappCommandInput } from "./generated/graphql";
import { GenesisAccount } from "./genesis";
import { convAuthRequiredToGqlType, fieldToHex } from "./utils";

export type RollupContext = {
  rollup: Rollup;
};

export enum CommandType {
  SignedCommand = 0,
  ZkappCommand = 1,
}

export type Transaction = {
  id: string;
  commandType: CommandType;
};

export type Batch = {
  ledgerHash: string;
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

  constructor(genesisAccounts: GenesisAccount[], accountCreationFee: number | string) {
    this.ledger = Ledger.create();

    genesisAccounts.forEach(({ publicKey, balance }) => {
      this.ledger.addAccount(publicKey, balance.toString());
    });

    const dummyBlockchain = Mina.LocalBlockchain({
      accountCreationFee: accountCreationFee.toString(),
      proofsEnabled: true,
    });

    this.networkState = dummyBlockchain.getNetworkState();
    this.networkConstants = dummyBlockchain.getNetworkConstants();
  }

  getAccount(publicKey: MlPublicKey, token: FieldConst): Account | null {
    Test.tokenId;

    const acc = this.ledger.getAccount(publicKey, token);

    if (acc === undefined) return null;

    return {
      publicKey: acc.publicKey,
      balance: {
        blockHeight: this.networkState.blockchainLength,
        liquid: acc.balance,
        locked: "0",
        stateHash: this.networkState.snarkedLedgerHash,
        total: acc.balance,
        unknown: "0",
      },
      nonce: acc.nonce,
      inferredNonce: acc.nonce,
      permissions: {
        access: convAuthRequiredToGqlType(acc.permissions.access),
        editActionState: convAuthRequiredToGqlType(acc.permissions.editActionState),
        editState: convAuthRequiredToGqlType(acc.permissions.editState),
        incrementNonce: convAuthRequiredToGqlType(acc.permissions.incrementNonce),
        receive: convAuthRequiredToGqlType(acc.permissions.receive),
        send: convAuthRequiredToGqlType(acc.permissions.send),
        setDelegate: convAuthRequiredToGqlType(acc.permissions.setDelegate),
        setPermissions: convAuthRequiredToGqlType(acc.permissions.setPermissions),
        setTiming: convAuthRequiredToGqlType(acc.permissions.setTiming),
        setTokenSymbol: convAuthRequiredToGqlType(acc.permissions.setTokenSymbol),
        setVerificationKey: convAuthRequiredToGqlType(acc.permissions.setVerificationKey),
        setVotingFor: convAuthRequiredToGqlType(acc.permissions.setVotingFor),
        setZkappUri: convAuthRequiredToGqlType(acc.permissions.setZkappUri),
      },
      provedState: false, // TODO
      receiptChainHash: Base58Encodings.ReceiptChainHash.toBase58(Field(acc.receiptChainHash)),
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
      privateKeyPath: "",
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
      Test.encoding.publicKeyOfBase58(publicKey),
      Test.encoding.tokenIdOfBase58(tokenId)
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
        txStatus: "applied",
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
        txStatus: "applied",
        txMemo,
        authorizationKind,
      })),
      ...previousEvents,
    ]);
  }

  updateNetworkState() {
    this.networkState.blockchainLength = this.networkState.blockchainLength.add(1);
    this.networkState.globalSlotSinceGenesis = this.networkState.globalSlotSinceGenesis.add(1);
  }

  applyZkappCommand(zkappCommand: ZkappCommandInput): {
    hash: string;
    id: string;
  } {
    const result = this.ledger.applyZkappCommand(
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

      if (authorizationKind.isProved && authorizationKind.isSigned) authorizationKindString = "Either";
      else if (authorizationKind.isSigned) authorizationKindString = "Signature";
      else if (authorizationKind.isProved) authorizationKindString = "Proof";
      else authorizationKindString = "None";

      this.storeActions(publicKey, tokenId, result.hash, zkappCommand.memo, actions, authorizationKindString);
      this.storeEvents(publicKey, tokenId, result.hash, zkappCommand.memo, events, authorizationKindString);
    });

    this.updateNetworkState();

    this.stagedTransactions.push({
      ...result,
      commandType: CommandType.ZkappCommand,
    });

    return result;
  }

  applyPayment(signature: Signature, userCommand: SendPaymentInput): { hash: string; id: string } {
    const { from, to, amount, fee, validUntil, nonce, memo } = userCommand;

    const result = this.ledger.applyPayment(
      signature.toBase58(),
      from,
      to,
      amount.toString(),
      fee.toString(),
      validUntil.toString(),
      nonce.toString(),
      Test.encoding.memoToBase58(memo?.toString() ?? ""),
      this.networkConstants.accountCreationFee.toString(),
      JSON.stringify(this.networkState)
    );

    this.updateNetworkState();

    this.stagedTransactions.push({
      ...result,
      commandType: CommandType.SignedCommand,
    });

    return result;
  }

  applyBatch(batch: Batch): void {
    batch.transactions.forEach((tx) => {
      switch (tx.commandType) {
        case CommandType.SignedCommand:
          this.ledger.applyPaymentFromBase64(
            tx.id,
            this.networkConstants.accountCreationFee.toString(),
            JSON.stringify(this.networkState)
          );

          break;
        case CommandType.ZkappCommand:
          this.ledger.applyZkappCommandFromBase64(
            tx.id,
            this.networkConstants.accountCreationFee.toString(),
            JSON.stringify(this.networkState)
          );
          break;
      }
    });

    this.batches.push(batch);
  }

  async bootstrap(lastBatchLedgerHash: string): Promise<void> {
    console.log("Bootstrapping the branch to: ", lastBatchLedgerHash);

    const batches = await fetchBatches(lastBatchLedgerHash);

    batches.forEach((batch) => {
      this.applyBatch(batch);
      console.log("Applied batch: ", batch.ledgerHash);
    });
  }

  async commit(): Promise<void> {
    const stagedTransactions = this.stagedTransactions.slice();
    this.stagedTransactions = [];

    const ledgerHash = fieldToHex(Field.random()); // TODO: get from ledger
    const previousLedgerHash = this.batches.at(-1)?.ledgerHash ?? ethers.constants.HashZero;

    const batch = {
      ledgerHash,
      transactions: stagedTransactions,
    };

    this.batches.push(batch);

    const signatures = await postBatch(batch, previousLedgerHash);

    console.log("Signatures collected for: ", batch.ledgerHash, signatures.length);
  }
}
