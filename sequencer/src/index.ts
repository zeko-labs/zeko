import { ApolloServer } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
import { Mina, PrivateKey } from "snarkyjs";
import config from "./config";
import { loadAccounts } from "./genesis";
import { RollupContext, loadSchema, resolvers } from "./gql";
import logger from "./logger";
import { createRollupContext } from "./rollup";

const run = async () => {
  const server = new ApolloServer<RollupContext>({
    typeDefs: await loadSchema("schema.graphql"),
    resolvers,
  });

  const Local = Mina.LocalBlockchain({
    proofsEnabled: true,
  });
  Mina.setActiveInstance(Local);

  const context = await createRollupContext(
    await loadAccounts(),
    Local.testAccounts[0].privateKey,
    PrivateKey.random()
  );

  if (config.DEPLOY_ZKAPP) {
    await context.rollup.deploy();
  }

  const { url } = await startStandaloneServer(server, {
    listen: { port: config.PORT },
    context: async () => context,
  });

  logger.info(`🚀  Server ready at: ${url}`);
};

run().catch(console.error);
