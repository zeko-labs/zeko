import { ApolloServer, BaseContext } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
import config from "./config";
import { loadSchema, resolvers } from "./gql";
import logger from "./logger";

const run = async () => {
  const server = new ApolloServer<BaseContext>({
    typeDefs: await loadSchema("schema.graphql"),
    resolvers,
  });

  const { url } = await startStandaloneServer(server, {
    listen: { port: config.PORT },
  });

  logger.info(`🚀  Server ready at: ${url}`);
};

run().catch(console.error);
