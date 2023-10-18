import { ethers } from "ethers";
import {
  AccountUpdate,
  Async_js,
  Base58Encodings,
  Field,
  FieldConst,
  Mina,
  MinaUtils,
  MlPublicKey,
  RollupBindings,
  RollupInstance,
  RollupMethods,
  Signature,
  Types,
  withThreadPool,
} from "o1js";
import { signerKey, zkappKey } from "./L1";
import config from "./config";
import { DALayer } from "./daLayer";
import { Account, SendPaymentInput } from "./generated/graphql";
import { GenesisAccount } from "./genesis";
import { RollupContext } from "./gql";
import logger from "./logger";
import { commandProver } from "./proving/command";
import { commitProver } from "./proving/commit";
import { bigIntStrToHex, convAuthRequiredToGqlType, minaToDecimal } from "./utils";

export const createRollupContext = async (genesisAccounts: GenesisAccount[]): Promise<RollupContext> => {
  Async_js.init();

  let mainThreadPoolStop: (() => void) | undefined = undefined;
  withThreadPool(() => new Promise<void>((resolve) => (mainThreadPoolStop = resolve)));

  const mlPk = MinaUtils.encoding.publicKeyOfBase58(zkappKey.toPublicKey().toBase58());

  commandProver.start(mlPk);
  commitProver.start(mlPk);

  await commandProver.startedPromise;
  await commitProver.startedPromise;

  logger.info("compiling rollup bindings");
  const bindings: RollupMethods = RollupBindings.compile(mlPk);
  logger.info("bindings ready");

  await commandProver.readyPromise;
  await commitProver.readyPromise;

  const daLayer = new DALayer();

  const rollup = new Rollup(daLayer, bindings, genesisAccounts);

  await rollup.deploy();
  await rollup.bootstrap();

  return {
    rollup,
    teardown: async () => {
      await commandProver.stop();
      await commitProver.stop();

      mainThreadPoolStop!();
      daLayer.stop();
    },
  };
};

export enum CommandType {
  SignedCommand = 0,
  ZkappCommand = 1,
}

export type Transaction = {
  id: string;
  commandType: CommandType;
};

export type StoredTransaction = {
  daIndex: number;
} & Transaction;

export class Rollup {
  public stagedTransactions: StoredTransaction[] = [];
  public committedTransactions: StoredTransaction[] = [];

  public lastCommittedBatchId: string = ethers.constants.HashZero;
  private isCommitting: boolean = false;

  public lastTxnSnarkPromise: Promise<string | undefined> = Promise.resolve(undefined);
  public currentTxnSnarkPromise: Promise<string | undefined> = Promise.resolve(undefined);

  private rollup: RollupInstance;
  private deployAccountUpdate: string;
  private genesisLedgerHash: string;

  constructor(public daLayer: DALayer, public bindings: RollupMethods, genesisAccounts: GenesisAccount[]) {
    const {
      rollup,
      accountUpdate: deployAccountUpdate,
      genesisLedgerHash,
    } = bindings.createZkapp("rollup", genesisAccounts);
    this.rollup = rollup;
    this.deployAccountUpdate = deployAccountUpdate;
    this.genesisLedgerHash = genesisLedgerHash;

    if (config.COMMITMENT_PERIOD !== 0) {
      setInterval(() => {
        logger.info("Committing staged transactions");
        this.commit();
      }, config.COMMITMENT_PERIOD);
    }
  }

  public getCommittedLedgerHash() {
    try {
      return Mina.getAccount(zkappKey.toPublicKey()).zkapp?.appState.at(0);
    } catch (e) {
      return undefined;
    }
  }

  public async deploy() {
    if (Mina.hasAccount(zkappKey.toPublicKey())) {
      logger.info("zkapp already deployed, skipping");
      return;
    }

    logger.info("Deploying zkapp");

    const deployTx = await Mina.transaction({ sender: signerKey.toPublicKey(), fee: 0.1e9 }, () => {
      AccountUpdate.fundNewAccount(signerKey.toPublicKey());
    });

    const snarkyjsDeployAccountUpdate = AccountUpdate.fromJSON(JSON.parse(this.deployAccountUpdate));
    snarkyjsDeployAccountUpdate.lazyAuthorization = { kind: "lazy-signature" };

    deployTx.transaction.accountUpdates.push(snarkyjsDeployAccountUpdate);

    await deployTx.prove();
    const txId = await deployTx.sign([signerKey, zkappKey]).send();

    logger.info("Deploy tx hash:", txId.hash(), "Success:", txId.isSuccess);

    await txId.wait();
  }

  public async commit() {
    if (this.isCommitting) {
      logger.info("Already committing, skipping commit");
      return;
    }

    if (this.stagedTransactions.length === 0) {
      logger.info("No staged transactions, skipping commit");
      return;
    }

    const transactionsToCommit = this.stagedTransactions.slice();

    this.isCommitting = true;
    this.committedTransactions.push(...transactionsToCommit);
    this.stagedTransactions = [];

    const txnSnarkToCommit = await this.currentTxnSnarkPromise;

    if (txnSnarkToCommit === undefined) {
      throw new Error("No snark to commit");
    }

    const batchId = bigIntStrToHex(this.bindings.getLedgerHashFromSnark(txnSnarkToCommit));

    await this.daLayer.postBatch(
      batchId,
      this.lastCommittedBatchId,
      transactionsToCommit.map((tx) => tx.daIndex)
    );

    const stepCallForest = await commitProver.enqueue({ txnSnark: txnSnarkToCommit });

    const stepTx = await Mina.transaction({ sender: signerKey.toPublicKey(), fee: 0.1e9 }, () => {});

    const stepAccountUpdates = (JSON.parse(stepCallForest) as Types.Json.AccountUpdate[]).map((au) =>
      AccountUpdate.fromJSON(au)
    );

    stepTx.transaction.accountUpdates.push(...stepAccountUpdates);

    const txId = await stepTx.sign([signerKey, zkappKey]).send();

    logger.info("Commit tx hash:", txId.hash(), "Success:", txId.isSuccess);

    await txId.wait();

    this.lastCommittedBatchId = batchId;
    this.isCommitting = false;

    logger.info("Committed");
  }

  public async bootstrap() {
    const committedLedgerHash = this.getCommittedLedgerHash();

    if (committedLedgerHash === undefined) {
      throw new Error("zkapp not deployed");
    }

    if (committedLedgerHash.toString() === this.genesisLedgerHash) {
      logger.info("nothing to boostrap, skipping");
      return;
    }

    logger.info("bootstrapping");

    const batchId = bigIntStrToHex(committedLedgerHash.toString());

    const batchOrderings = await this.daLayer.getBatchesTillGenesis(batchId);

    const commands = await Promise.all(
      batchOrderings.flat().map(async (commandDaIndex) => this.daLayer.getCommand(commandDaIndex))
    );

    commands.forEach((command) => {
      const { payload, signer, signature } = JSON.parse(MinaUtils.transactionHash.paymentOfBase64(command.id));

      this.bindings.applyUserCommand(this.rollup, {
        signature: signature,
        fromBase58: signer,
        toBase58: payload.body.at(1).receiver_pk,
        amount: payload.body.at(1).amount,
        fee: minaToDecimal(payload.common.fee).toString(),
        validUntil: payload.common.valid_until,
        nonce: payload.common.nonce,
        memo: payload.common.memo,
      });
    });

    this.lastCommittedBatchId = batchId;
    this.committedTransactions = commands;

    logger.info("bootstrapped to", this.bindings.getRoot(this.rollup).toString());
  }

  public getRoot() {
    return Field.from(this.bindings.getRoot(this.rollup));
  }

  public getAccount(publicKey: MlPublicKey, token: FieldConst): Account | null {
    const acc = this.bindings.getAccount(this.rollup, publicKey, token);

    if (acc === undefined) return null;

    return {
      publicKey: acc.publicKey,
      balance: {
        blockHeight: this.rollup.slot,
        liquid: acc.balance,
        locked: "0",
        stateHash: "",
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
      provedState: false,
      index: null,
      leafHash: null,
      locked: false,
      merklePath: null,
      privateKeyPath: "",
    };
  }

  public async applyPayment(
    signature: Signature,
    userCommand: SendPaymentInput
  ): Promise<{ hash: string; id: string }> {
    const { from, to, amount, fee, validUntil, nonce, memo } = userCommand;

    const { txId, txHash, txnSnarkInputJson } = this.bindings.applyUserCommand(this.rollup, {
      signature: signature.toBase58(),
      fromBase58: from,
      toBase58: to,
      amount: amount.toString(),
      fee: fee.toString(),
      validUntil: validUntil.toString(),
      nonce: nonce.toString(),
      memo: MinaUtils.encoding.memoToBase58(memo?.toString() ?? ""),
    });

    const tx = {
      id: txId,
      commandType: CommandType.SignedCommand,
    };

    const daIndex = await this.daLayer.postCommand(tx);

    const prevSnarkPromise = this.lastTxnSnarkPromise;
    this.lastTxnSnarkPromise = prevSnarkPromise.then((prevSnarkJson) => {
      this.stagedTransactions.push({ ...tx, daIndex });

      this.currentTxnSnarkPromise = commandProver.enqueue({
        snarkInp: txnSnarkInputJson,
        prevSnark: this.stagedTransactions.length !== 1 ? prevSnarkJson : undefined,
      });

      return this.currentTxnSnarkPromise;
    });

    return { hash: txHash, id: txId };
  }
}
