import { FieldConst, MinaUtils } from "snarkyjs";
import { RollupContext } from ".";
import { Account, DaemonStatus, Peer, QueryResolvers, SyncStatus, TransactionStatus } from "../generated/graphql";

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

  account(_, { publicKey, token }, { rollup }: RollupContext): Account | null {
    return rollup.getAccount(
      MinaUtils.encoding.publicKeyOfBase58(publicKey),
      token !== undefined ? MinaUtils.encoding.tokenIdOfBase58(token) : FieldConst[1]
    );
  },

  transactionStatus(_, { zkappTransaction, payment }, { rollup }: RollupContext) {
    if (rollup.stagedTransactions.some((t) => t.id === zkappTransaction || t.id === payment)) {
      return TransactionStatus.Pending;
    }

    if (rollup.committedTransactions.some((t) => t.id === zkappTransaction || t.id === payment)) {
      return TransactionStatus.Included;
    }

    return TransactionStatus.Unknown;
  },

  bestChain() {
    return [];
  },
};
