import { AccountUpdate, Mina, PublicKey, Types, fetchAccount } from "o1js";
import env from "./configs/env";
import { WrappingDirection } from "./utils/types";

type Transaction = Awaited<ReturnType<typeof Mina.transaction>>;

// ---------------------------------------------------------------------------------------

const state = {
  transaction: null as null | Transaction,
};

// ---------------------------------------------------------------------------------------

const functions = {
  setActiveInstanceToBerkeley: async (args: {}) => {
    const Berkeley = Mina.Network(
      "https://proxy.berkeley.minaexplorer.com/graphql"
    );
    console.log("Berkeley Instance Created");
    Mina.setActiveInstance(Berkeley);
  },
  fetchAccount: async (args: { publicKey58: string }) => {
    const publicKey = PublicKey.fromBase58(args.publicKey58);
    const res = await fetchAccount({ publicKey });
    return {
      account:
        res.error == null
          ? Types.Account.toJSON((await fetchAccount({ publicKey })).account!)
          : undefined,
      error: res.error,
    };
  },
  createBridgeTransaction: async ({
    amount,
    direction,
    sender,
    target,
  }: {
    amount: bigint;
    direction: WrappingDirection;
    sender: PublicKey;
    target: string;
  }) => {
    // Send request to prove transfer
    const response = await fetch(env.REACT_APP_SEQUENCER_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        query: `
        mutation ($transferInput: TransferInput!) {
          proveTransfer(input: $transferInput) {
            accountUpdateKey
          }
        }
      `,
        variables: {
          transferInput: {
            address: target,
            amount: amount.toString(),
            direction,
          },
        },
      }),
    });

    const key = (await response.json()).data.proveTransfer.accountUpdateKey;

    // Poll for proved account update
    let transferCallForest: string | null = null;
    while (true) {
      console.log("Polling for transferJson");
      const response = await fetch(env.REACT_APP_SEQUENCER_URL, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          query: `
          query ($key: String!) {
            transfer(key: $key)
          }
        `,
          variables: { key },
        }),
      });

      transferCallForest = (await response.json()).data.transfer;

      if (transferCallForest !== null) {
        break;
      } else {
        await new Promise((resolve) => setTimeout(resolve, 3_000));
      }
    }

    // Create zkapp command
    let txn = await Mina.transaction(sender, () => {
      AccountUpdate.fundNewAccount(sender);

      const fundsUpdate = AccountUpdate.createSigned(sender);
      fundsUpdate.balance.subInPlace(amount);
    });

    // Append proved account update to the command
    JSON.parse(transferCallForest).forEach((accountUpdate: any) => {
      txn.transaction.accountUpdates.push(
        AccountUpdate.fromJSON(accountUpdate)
      );
    });

    return txn.toJSON();
  },
};

// ---------------------------------------------------------------------------------------

export type WorkerFunctions = keyof typeof functions;

export type ZkappWorkerRequest = {
  id: number;
  fn: WorkerFunctions;
  args: any;
};

export type ZkappWorkerReponse = {
  id: number;
  data: any;
};

// eslint-disable-next-line no-restricted-globals
self.onmessage = async (event: MessageEvent<ZkappWorkerRequest>) => {
  const returnData = await functions[event.data.fn](event.data.args);

  const message: ZkappWorkerReponse = {
    id: event.data.id,
    data: returnData,
  };
  postMessage(message);
};

console.log("Web Worker Successfully Initialized.");
