import { ApolloClient, InMemoryCache, NormalizedCacheObject, gql } from "@apollo/client/core";
import { WebSocketLink } from "@apollo/client/link/ws";
import { Mina, PrivateKey, fetchAccount } from "o1js";
import ws from "ws";
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

let gqlClient: ApolloClient<NormalizedCacheObject> | null = null;

if (config.MINA_NODE_URL === "") {
  Mina.setActiveInstance(Local);
} else {
  const Network = Mina.Network(config.MINA_NODE_URL);
  Mina.setActiveInstance(Network);

  fetchAccount({ publicKey: signerKey.toPublicKey() });
  fetchAccount({ publicKey: zkappKey.toPublicKey() });

  const link = new WebSocketLink({
    uri: config.MINA_NODE_URL,
    options: {
      reconnect: true,
    },
    webSocketImpl: ws,
  });

  link.setOnError(console.error);

  gqlClient = new ApolloClient({ cache: new InMemoryCache(), link });
}

export const onChainReorder = (callback: () => void) => {
  if (gqlClient === null) {
    return;
  }

  gqlClient
    .subscribe({
      query: gql`
        subscription sub {
          chainReorganization
        }
      `,
    })
    .forEach(callback)
    .catch(logger.error);
};
