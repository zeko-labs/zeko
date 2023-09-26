import {
  AccountUpdate,
  Async_js,
  Base58Encodings,
  Field,
  FieldConst,
  Mina,
  MinaUtils,
  MlPublicKey,
  PrivateKey,
  RollupBindings,
  RollupInstance,
  RollupMethods,
  Signature,
  Types,
  withThreadPool,
} from "snarkyjs";
import config from "./config";
import { DALayer } from "./daLayer";
import { Account, SendPaymentInput } from "./generated/graphql";
import { GenesisAccount } from "./genesis";
import { RollupContext } from "./gql";
import logger from "./logger";
import { commandProver } from "./proving/command";
import { commitProver } from "./proving/commit";
import { convAuthRequiredToGqlType } from "./utils";

export const createRollupContext = async (
  genesisAccounts: GenesisAccount[],
  signer: PrivateKey,
  zkappKey: PrivateKey
): Promise<RollupContext> => {
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

  const rollup = new Rollup(zkappKey, signer, daLayer, bindings, genesisAccounts);

  // logger.info("Bootstrapping");
  // await rollupState.bootstrap(await fetchCurrentLedgerHash());

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

export class Rollup {
  public stagedTransactions: Transaction[] = [];
  public committedTransactions: Transaction[] = [];

  public lastTxnSnark: string = "";
  public txnSnarkPromise: Promise<string> = Promise.resolve(this.lastTxnSnark);

  private rollup: RollupInstance;
  private deployAccountUpdate: string;

  constructor(
    public zkappKey: PrivateKey,
    public signer: PrivateKey,
    public daLayer: DALayer,
    public bindings: RollupMethods,
    genesisAccounts: GenesisAccount[]
  ) {
    const { rollup, accountUpdate: deployAccountUpdate } = bindings.createZkapp("rollup", genesisAccounts);
    this.rollup = rollup;
    this.deployAccountUpdate = deployAccountUpdate;

    if (config.COMMITMENT_PERIOD !== 0) {
      setInterval(() => {
        if (this.stagedTransactions.length === 0) return;

        logger.info("Committing staged transactions");
        this.commit();
      }, config.COMMITMENT_PERIOD);
    }
  }

  public async deploy() {
    const deployTx = await Mina.transaction(this.signer.toPublicKey(), () => {
      AccountUpdate.fundNewAccount(this.signer.toPublicKey());
    });

    const snarkyjsDeployAccountUpdate = AccountUpdate.fromJSON(JSON.parse(this.deployAccountUpdate));
    snarkyjsDeployAccountUpdate.lazyAuthorization = { kind: "lazy-signature" };

    deployTx.transaction.accountUpdates.push(snarkyjsDeployAccountUpdate);

    await deployTx.prove();
    await deployTx.sign([this.signer, this.zkappKey]).send();
  }

  public async commit() {
    this.committedTransactions.push(...this.stagedTransactions);
    this.stagedTransactions = [];

    const stepCallForest = await commitProver.enqueue({ txnSnark: this.lastTxnSnark });

    const stepTx = await Mina.transaction(this.signer.toPublicKey(), () => {});

    const stepAccountUpdates = (JSON.parse(stepCallForest) as Types.Json.AccountUpdate[]).map((au) =>
      AccountUpdate.fromJSON(au)
    );

    stepTx.transaction.accountUpdates.push(...stepAccountUpdates);

    await stepTx.sign([this.signer, this.zkappKey]).send();
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

    this.stagedTransactions.push(tx);

    await this.daLayer.postCommand(tx);

    const prevSnarkPromise = this.txnSnarkPromise;
    this.txnSnarkPromise = prevSnarkPromise.then((prevSnarkJson) => {
      return commandProver.enqueue({ snarkInp: txnSnarkInputJson, prevSnark: prevSnarkJson });
    });

    this.txnSnarkPromise.then((txnSnarkJson) => {
      this.lastTxnSnark = txnSnarkJson;
    });

    return { hash: txHash, id: txId };
  }
}
