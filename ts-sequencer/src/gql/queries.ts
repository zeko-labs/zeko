import { Field, PublicKey } from 'snarkyjs';
import {
  Account,
  DaemonStatus,
  Peer,
  QueryResolvers,
  SyncStatus,
} from '../generated/graphql';
import { RollupContext } from '../rollup';

export const queries: QueryResolvers = {
  syncStatus() {
    return SyncStatus.Synced;
  },

  daemonStatus() {
    return {
      chainId: '1337',
      syncStatus: SyncStatus.Synced,
      peers: [] as Peer[],
      highestBlockLengthReceived: 0,
      highestUnvalidatedBlockLengthReceived: 0,
    } as DaemonStatus;
  },

  account(_, { publicKey, token }, { rollup }: RollupContext): Account | null {
    return rollup.getAccount(
      PublicKey.fromBase58(publicKey),
      Field(token ?? 1)
    );
  },
};
