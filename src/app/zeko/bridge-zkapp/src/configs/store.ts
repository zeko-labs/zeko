import { create } from "zustand";
import { PublicKey, Types } from "o1js";
import ZkappWorkerClient from "../zkappWorkerClient";
import { timeout } from "../utils/helper";

type State = {
  userPublicKey: PublicKey | undefined;
  account: Types.Account | undefined;
  workerClient: ZkappWorkerClient | undefined;
};

type Actions = {
  setAccount: (account: Types.Account | undefined) => Promise<void>;
  setUserPublicKey: (publicKey: PublicKey | undefined) => Promise<void>;
  initWorkerClient: () => Promise<void>;
};

export const useAppStore = create<State & Actions>((set) => ({
  account: undefined,
  userPublicKey: undefined,
  workerClient: undefined,
  setAccount: async (account: Types.Account | undefined) => {
    set(() => ({ account }));
  },
  setUserPublicKey: async (publicKey: PublicKey | undefined) => {
    set(() => ({ userPublicKey: publicKey }));
  },
  initWorkerClient: async () => {
    console.log("Loading web worker...");
    const zkappWorkerClient = new ZkappWorkerClient();

    await timeout(5);
    console.log("Done loading web worker");
    set(() => ({ workerClient: zkappWorkerClient }));
    await zkappWorkerClient.setActiveInstanceToBerkeley();
  },
}));
