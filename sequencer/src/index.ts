import { ApolloServer } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
import fs from "fs/promises";
import { fetchCurrentLedgerHash } from "./L1";
import config from "./config";
import { Resolvers } from "./generated/graphql";
import { loadAccounts } from "./genesis";
import { mutations } from "./gql/mutations";
import { queries } from "./gql/queries";
import { Rollup } from "./rollup";
import { minaToDecimal } from "./utils";

type RollupContext = {
  rollup: Rollup;
};

const resolvers: Resolvers = {
  query: queries,
  mutation: mutations,
};

const loadSchema = async (fileName: string): Promise<string> => {
  return fs.readFile(fileName, "utf-8");
};

const run = async () => {
  const rollupState = new Rollup(await loadAccounts(), minaToDecimal(1));

  await rollupState.bootstrap(await fetchCurrentLedgerHash());

  setInterval(() => {
    if (rollupState.stagedTransactions.length === 0) return;

    console.log("Committing staged transactions");
    rollupState.commit();
  }, config.COMMITMENT_PERIOD);

  const server = new ApolloServer<RollupContext>({
    typeDefs: await loadSchema("schema.graphql"),
    resolvers,
  });

  const { url } = await startStandaloneServer(server, {
    listen: { port: config.PORT },
    context: async () => ({
      rollup: rollupState,
    }),
  });

  console.log(`🚀  Server ready at: ${url}`);
};

run().catch(console.error);
