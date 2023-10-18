import { Mina, PrivateKey, fetchAccount } from "o1js";
import config from "./config";
import logger from "./logger";

const Local = Mina.LocalBlockchain({
  proofsEnabled: true,
});

export const signerKey =
  config.MINA_SIGNER_PRIVATE_KEY !== ""
    ? PrivateKey.fromBase58(config.MINA_SIGNER_PRIVATE_KEY)
    : Local.testAccounts[0].privateKey;

export const zkappKey =
  config.MINA_ZKAPP_PRIVATE_KEY !== "" ? PrivateKey.fromBase58(config.MINA_ZKAPP_PRIVATE_KEY) : PrivateKey.random();

logger.info("Signer public key: " + signerKey.toPublicKey().toBase58());
logger.info("Zkapp public key: " + zkappKey.toPublicKey().toBase58());

if (config.MINA_NODE_URL !== "") {
  const Network = Mina.Network(config.MINA_NODE_URL);
  Mina.setActiveInstance(Network);

  fetchAccount({ publicKey: signerKey.toPublicKey() });
  fetchAccount({ publicKey: zkappKey.toPublicKey() });
} else {
  Mina.setActiveInstance(Local);
}
