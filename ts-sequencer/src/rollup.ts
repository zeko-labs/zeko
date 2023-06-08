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

export class Rollup {
  public ledger: Ledger;

  public readonly networkState;
  public readonly networkConstants;

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
    return this.ledger.applyJsonTransaction(
      JSON.stringify(zkappCommand),
      this.networkConstants.accountCreationFee.toString(),
      JSON.stringify(this.networkState)
    );
  }

  applyPayment(
    signature: Signature,
    { from, to, amount, fee, validUntil, nonce, memo }: SendPaymentInput
  ): { hash: string; id: string } {
    return this.ledger.applyPayment(
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
  }
}
