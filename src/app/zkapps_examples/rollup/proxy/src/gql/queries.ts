import {
  Account,
  DaemonStatus,
  Peer,
  QueryResolvers,
  SyncStatus,
} from "../generated/graphql";

export const queries: QueryResolvers = {
  syncStatus() {
    return SyncStatus.Synced;
  },

  daemonStatus() {
    return {
      chainId: "69420",
      syncStatus: SyncStatus.Synced,
      peers: [] as Peer[],
      highestBlockLengthReceived: 0,
      highestUnvalidatedBlockLengthReceived: 0,
    } as DaemonStatus;
  },

  account(_, { publicKey, token }): Account | null {
    throw new Error("Not implemented");
  },

  transactionStatus(_, { zkappTransaction, payment }) {
    throw new Error("Not implemented");
  },

  bestChain() {
    return [];
  },
};
