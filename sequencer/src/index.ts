import { ApolloServer } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
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

  const context = await createRollupContext(await loadAccounts());

  const { url } = await startStandaloneServer(server, {
    listen: { port: config.PORT },
    context: async () => context,
  });

  logger.info(`🚀  Server ready at: ${url}`);
};

run().catch(console.error);
