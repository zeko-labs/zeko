import { ApolloServer } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
import fs from "fs/promises";
import { Test } from "snarkyjs";
import config from "./config";
import { daLayerContract } from "./daLayer";
import { Resolvers } from "./generated/graphql";
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
  const rollupState = new Rollup(
    [
      {
        publicKey: Test.encoding.publicKeyOfBase58("B62qnPZzpnQWA8FLBn9qqJqPTeGuDdHTZgpmEMUNFCq8fWCRSqJS6Jd"),
        balance: minaToDecimal(1_000),
      },
      {
        publicKey: Test.encoding.publicKeyOfBase58("B62qm8mVEkhAZdnnoE4gzKhTYj2Gre88GNhoknbJdgDrE8JQojNBgWa"),
        balance: minaToDecimal(2_000),
      },
      {
        publicKey: Test.encoding.publicKeyOfBase58("B62qrrytZmo8SraqYfJMZ8E3QcK77uAGZhsGJGKmVF5E598E8KX9j6a"),
        balance: minaToDecimal(3_000),
      },
      {
        publicKey: Test.encoding.publicKeyOfBase58("B62qkJbh5dsbPFq6kn1SCmgSfeamgdnnNV9Jqf7hj7BPyRBNPMbuTfC"),
        balance: minaToDecimal(4_000),
      },
    ],
    minaToDecimal(1)
  );

  await rollupState.bootstrap(await daLayerContract.lastProposedBatch());

  setInterval(() => {
    if (rollupState.stagedTransactions.length === 0) return;

    console.log("Committing staged transactions");
    rollupState.commit();
  }, 10_000);

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
