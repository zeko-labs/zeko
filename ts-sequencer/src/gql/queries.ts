import { Base58Encodings, Field, PublicKey } from 'snarkyjs';
import {
  Account,
  DaemonStatus,
  Peer,
  QueryResolvers,
  SyncStatus,
  TransactionStatus,
} from '../generated/graphql';
import { RollupContext } from '../rollup';

export const queries: QueryResolvers = {
  syncStatus() {
    return SyncStatus.Synced;
  },

  daemonStatus() {
    return {
      chainId: '69420',
      syncStatus: SyncStatus.Synced,
      peers: [] as Peer[],
      highestBlockLengthReceived: 0,
      highestUnvalidatedBlockLengthReceived: 0,
    } as DaemonStatus;
  },

  account(_, { publicKey, token }, { rollup }: RollupContext): Account | null {
    return rollup.getAccount(
      PublicKey.fromBase58(publicKey),
      token !== undefined ? Base58Encodings.TokenId.fromBase58(token) : Field(1)
    );
  },

  transactionStatus(
    _,
    { zkappTransaction, payment },
    { rollup }: RollupContext
  ) {
    if (
      rollup.stagedTransactions.some(
        (t) => t.id === zkappTransaction || t.id === payment
      )
    ) {
      return TransactionStatus.Pending;
    }

    if (
      rollup.batches
        .map(({ transactions }) => transactions)
        .flat()
        .some((t) => t.id === zkappTransaction || t.id === payment)
    ) {
      return TransactionStatus.Included;
    }

    return TransactionStatus.Unknown;
  },

  bestChain() {
    return [];
  },

  events() {
    return [];
  },

  actions() {
    return [];
  },
};
