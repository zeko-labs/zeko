import { AccountUpdate, Mina, PublicKey } from "o1js";
import env from "../configs/env";
import { useAppStore } from "../configs/store";
import { timeout } from "../utils/helper";
import { useMutation } from "@tanstack/react-query";
import ZkappWorkerClient from "../zkappWorkerClient";
import { TransactionStatus, WrappingDirection } from "../utils/types";
import useStoreTransaction from "./useStoreTransaction";

let transactionFee = 0.1;
type BridgeOperationParams = {
  amount: bigint;
  direction: WrappingDirection;
  sender: PublicKey;
  target: string;
};
const startBridge = async (
  { amount, direction, sender, target }: BridgeOperationParams,
  workerClient: ZkappWorkerClient
) => {
  /*
    const transactionJSON = await workerClient!.createBridgeTransaction({
      amount,
      direction,
      sender,
      target,
    });

    const { hash } = await window.mina!.sendTransaction({
      transaction: transactionJSON,
      feePayer: {
        fee: transactionFee,
        memo: "",
      },
    });

    const transactionLink = `https://berkeley.minaexplorer.com/transaction/${hash}`;
    console.log(`View transaction at ${transactionLink}`);
    */
  await timeout(5);
  const hash = (Math.random() + 1).toString(36).substring(2);
  return hash;
};

export default function useBridge() {
  const [workerClient] = useAppStore((state) => [state.workerClient]);
  const { mutate: storeTransaction } = useStoreTransaction();

  return useMutation({
    mutationFn: (params: BridgeOperationParams) => {
      if (!workerClient) {
        throw new Error("WorkerClient not initialized");
      }
      return startBridge(params, workerClient);
    },
    onSuccess: (hash, params) => {
      storeTransaction({
        id: hash,
        direction: params.direction,
        amount: params.amount.toString(),
        status: TransactionStatus.PENDING,
      });
    },
  });
}
