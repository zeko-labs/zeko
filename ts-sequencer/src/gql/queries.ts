import { Field, PublicKey } from 'snarkyjs';
import {
  Account,
  DaemonStatus,
  QueryResolvers,
  SyncStatus,
} from '../generated/graphql';
import { RollupContext } from '../rollup';

export const queries: QueryResolvers = {
  syncStatus() {
    return SyncStatus.Synced;
  },

  daemonStatus() {
    // TODO: return actual daemon status
    return { chainId: '1337' } as DaemonStatus;
  },

  account(_, { publicKey, token }, { rollup }: RollupContext): Account | null {
    const acc = rollup.ledger.getAccount(
      PublicKey.fromBase58(publicKey),
      Field(token ?? 1)
    );

    if (acc === undefined) return null;

    return {
      publicKey: acc.publicKey,
      balance: {
        blockHeight: rollup.networkState.blockchainLength,
        liquid: acc.balance,
        locked: '0',
        stateHash: rollup.networkState.snarkedLedgerHash,
        total: acc.balance,
        unknown: '0',
      },
      nonce: acc.nonce,
      inferredNonce: acc.nonce,
      permissions: null,
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
  },
};
