import { Mina, PrivateKey } from "o1js";
import config from "./config";

const Local = Mina.LocalBlockchain({
  proofsEnabled: true,
});

if (config.MINA_NODE_URL !== "") {
  const Network = Mina.Network(config.MINA_NODE_URL);
  Mina.setActiveInstance(Network);
} else {
  Mina.setActiveInstance(Local);
}

export const signerKey =
  config.MINA_SIGNER_PRIVATE_KEY !== ""
    ? PrivateKey.fromBase58(config.MINA_SIGNER_PRIVATE_KEY)
    : Local.testAccounts[0].privateKey;

export const zkappKey =
  config.MINA_ZKAPP_PRIVATE_KEY !== "" ? PrivateKey.fromBase58(config.MINA_ZKAPP_PRIVATE_KEY) : PrivateKey.random();
