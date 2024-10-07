import { fetchAccount, PublicKey, Types } from "o1js";

import type {
  ZkappWorkerRequest,
  ZkappWorkerReponse,
  WorkerFunctions,
} from "./zkappWorker";
import { WrappingDirection } from "./utils/types";

export default class ZkappWorkerClient {
  // ---------------------------------------------------------------------------------------

  setActiveInstanceToBerkeley() {
    return this._call("setActiveInstanceToBerkeley", {});
  }

  async fetchAccount({ publicKey }: { publicKey: PublicKey }) {
    const result = await (this._call("fetchAccount", {
      publicKey58: publicKey.toBase58(),
    }) as ReturnType<typeof fetchAccount>);
    if (result.account) {
      return {
        account: Types.Account.fromJSON(result.account as any),
        error: undefined,
      };
    }
    return {
      account: undefined,
      error: result.error,
    };
  }

  async createBridgeTransaction({
    amount,
    direction,
    sender,
    target,
  }: {
    amount: bigint;
    direction: WrappingDirection;
    sender: PublicKey;
    target: string;
  }) {
    const result = await this._call("createBridgeTransaction", {
      amount,
      direction,
      sender,
      target,
    });
    return result;
  }

  // ---------------------------------------------------------------------------------------

  worker: Worker;

  promises: {
    [id: number]: { resolve: (res: any) => void; reject: (err: any) => void };
  };

  nextId: number;

  constructor() {
    this.worker = new Worker(new URL("./zkappWorker.ts", import.meta.url));
    this.promises = {};
    this.nextId = 0;

    this.worker.onmessage = (event: MessageEvent<ZkappWorkerReponse>) => {
      this.promises[event.data.id].resolve(event.data.data);
      delete this.promises[event.data.id];
    };
  }

  _call(fn: WorkerFunctions, args: any) {
    return new Promise((resolve, reject) => {
      this.promises[this.nextId] = { resolve, reject };

      const message: ZkappWorkerRequest = {
        id: this.nextId,
        fn,
        args,
      };

      this.worker.postMessage(message);

      this.nextId++;
    });
  }
}
