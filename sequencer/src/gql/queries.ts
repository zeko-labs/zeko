import { FieldConst, Test } from "snarkyjs";
import { Account, DaemonStatus, Peer, QueryResolvers, SyncStatus, TransactionStatus } from "../generated/graphql";
import { RollupContext } from "../rollup";

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
      Test.encoding.publicKeyOfBase58(publicKey),
      token !== undefined ? Test.encoding.tokenIdOfBase58(token) : FieldConst[1]
    );
  },

  transactionStatus(_, { zkappTransaction, payment }, { rollup }: RollupContext) {
    if (rollup.stagedTransactions.some((t) => t.id === zkappTransaction || t.id === payment)) {
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

  events(_, { input }, { rollup }: RollupContext) {
    const { address, tokenId } = input;

    const events = rollup.events.get(`${address}-${tokenId ?? Test.encoding.tokenIdToBase58(FieldConst[1])}`);

    return (
      events?.map((event) => ({
        eventData: [
          {
            data: event.data,
            transactionInfo: {
              hash: event.txHash,
              status: event.txStatus,
              memo: event.txMemo,
              authorizationKind: event.authorizationKind,
            },
          },
        ],
        blockInfo: {
          height: 0,
          stateHash: "",
          parentHash: "",
          ledgerHash: "",
          chainStatus: "",
          timestamp: "",
          globalSlotSinceHardfork: 0,
          globalSlotSinceGenesis: 0,
          distanceFromMaxBlockHeight: 1, // due to https://github.com/o1-labs/snarkyjs/blob/main/src/lib/fetch.ts#L847-L852
        },
      })) ?? []
    );
  },

  actions(_, { input }, { rollup }: RollupContext) {
    const { address, tokenId } = input;

    const actions = rollup.actions.get(`${address}-${tokenId ?? Test.encoding.tokenIdToBase58(FieldConst[1])}`);

    return (
      actions?.map((action) => ({
        actionState: {
          actionStateOne: action.state.at(0),
          actionStateTwo: action.state.at(1),
          actionStateThree: action.state.at(2),
          actionStateFour: action.state.at(3),
          actionStateFive: action.state.at(4),
        },
        actionData: [
          {
            accountUpdateId: action.accountUpdateId.toString(),
            data: action.data,
            transactionInfo: {
              hash: action.txHash,
              status: action.txStatus,
              memo: action.txMemo,
              authorizationKind: action.authorizationKind,
            },
          },
        ],
        blockInfo: {
          height: 0,
          stateHash: "",
          parentHash: "",
          ledgerHash: "",
          chainStatus: "",
          timestamp: "",
          globalSlotSinceHardfork: 0,
          globalSlotSinceGenesis: 0,
          distanceFromMaxBlockHeight: 1, // due to https://github.com/o1-labs/snarkyjs/blob/main/src/lib/fetch.ts#L847-L852
        },
      })) ?? []
    );
  },
};
