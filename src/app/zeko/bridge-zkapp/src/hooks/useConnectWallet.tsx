import { PublicKey } from "o1js";
import { useAppStore } from "../configs/store";

const KNOWN_ERROR_CODES: number[] = [20005, 1002];

export default function useConnectWallet() {
  const [setUserPublicKey, setAccount] = useAppStore((state) => [
    state.setUserPublicKey,
    state.setAccount,
  ]);
  async function connectWallet() {
    const mina = window.mina;
    if (!mina) {
      return;
    }
    try {
      setAccount(undefined);
      setUserPublicKey(undefined);
      const publicKeyBase58: string = (await mina.requestAccounts())[0];
      const publicKey = PublicKey.fromBase58(publicKeyBase58);
      setUserPublicKey(publicKey);
      return publicKey;
    } catch (err: any) {
      // Suppress known wallet errors
      if (err.code && KNOWN_ERROR_CODES.includes(err.code)) {
        console.log(err.message);
        return;
      }
      throw err;
    }
  }
  return { connectWallet };
}
