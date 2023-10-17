import MinaSigner from "mina-signer";
import { PrivateKey, PublicKey } from "o1js";

const signer = new MinaSigner({ network: "testnet" });

export const generateCommand = (from: PrivateKey, to: PublicKey, amount: number, nonce: number) => {
  return signer.signPayment(
    {
      to: to.toBase58(),
      from: from.toPublicKey().toBase58(),
      amount: amount,
      fee: 10,
      nonce,
      validUntil: Math.floor(+new Date() / 1000) + 1000,
      memo: "",
    },
    from.toBase58()
  );
};
