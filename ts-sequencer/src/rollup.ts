import { Field, Ledger, Mina, PublicKey, Signature } from 'snarkyjs';
import {
  Account,
  SendPaymentInput,
  ZkappCommandInput,
} from './generated/graphql';
import { authRequiredToGql } from './utils';

export type GenesisAccount = {
  publicKey: PublicKey;
  balance: number | string;
};

export type RollupContext = {
  rollup: Rollup;
};

export type Transaction = {
  hash: string;
  id: string;
  data: SendPaymentInput | ZkappCommandInput;
};

export class Rollup {
  public ledger: Ledger;

  public readonly networkState;
  public readonly networkConstants;

  public stagedTransactions: Transaction[] = [];
  public includedTransactions: Transaction[] = [];

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
        access: authRequiredToGql(acc.permissions.access),
        editActionState: authRequiredToGql(acc.permissions.editActionState),
        editState: authRequiredToGql(acc.permissions.editState),
        incrementNonce: authRequiredToGql(acc.permissions.incrementNonce),
        receive: authRequiredToGql(acc.permissions.receive),
        send: authRequiredToGql(acc.permissions.send),
        setDelegate: authRequiredToGql(acc.permissions.setDelegate),
        setPermissions: authRequiredToGql(acc.permissions.setPermissions),
        setTiming: authRequiredToGql(acc.permissions.setTiming),
        setTokenSymbol: authRequiredToGql(acc.permissions.setTokenSymbol),
        setVerificationKey: authRequiredToGql(
          acc.permissions.setVerificationKey
        ),
        setVotingFor: authRequiredToGql(acc.permissions.setVotingFor),
        setZkappUri: authRequiredToGql(acc.permissions.setZkappUri),
      },
      provedState: true,
      receiptChainHash: acc.receiptChainHash,
      timing: acc.timing,

      tokenId: acc.tokenId,
      token: acc.tokenId,
      tokenSymbol: acc.tokenSymbol,

      verificationKey: {
        hash: acc.zkapp?.verificationKey?.hash,
        verificationKey: acc.zkapp?.verificationKey?.data,
      },
      zkappState: acc.zkapp?.appState,
      zkappUri: acc.zkapp?.zkappUri,

      actionState: acc.zkapp?.actionState,
      index: null,
      leafHash: null,
      locked: false,
      merklePath: null,
      privateKeyPath: '',

      delegate: null,
      delegateAccount: null,
      delegators: null,
      lastEpochDelegators: null,
      stakingActive: false,
      votingFor: null,
    };
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

    this.stagedTransactions.push({ ...result, data: zkappCommand });

    return result;
  }

  applyPayment(
    signature: Signature,
    userCommand: SendPaymentInput
  ): { hash: string; id: string } {
    const { from, to, amount, fee, validUntil, nonce, memo } = userCommand;

    const result = this.ledger.applyPayment(
      signature.toBase58(),
      PublicKey.fromBase58(from),
      PublicKey.fromBase58(to),
      amount.toString(),
      fee.toString(),
      validUntil.toString(),
      nonce.toString(),
      memo?.toString() ?? '',
      this.networkConstants.accountCreationFee.toString(),
      JSON.stringify(this.networkState)
    );

    this.stagedTransactions.push({ ...result, data: userCommand });

    return result;
  }

  commit(): void {
    this.stagedTransactions.forEach((tx) => {
      this.includedTransactions.push(tx);
    });

    this.stagedTransactions = [];
  }
}
