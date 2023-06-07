import { ApolloServer } from '@apollo/server';
import { startStandaloneServer } from '@apollo/server/standalone';
import fs from 'fs/promises';
import { PublicKey } from 'snarkyjs';
import { Resolvers } from './generated/graphql';
import { mutations } from './gql/mutations';
import { queries } from './gql/queries';
import { Rollup } from './rollup';

type RollupContext = {
  rollup: Rollup;
};

const resolvers: Resolvers = {
  query: queries,
  mutation: mutations,
};

const loadSchema = async (fileName: string): Promise<string> => {
  return fs.readFile(fileName, 'utf-8');
};

const run = async () => {
  const rollupState = new Rollup(
    [
      {
        publicKey: PublicKey.fromBase58(
          'B62qnPZzpnQWA8FLBn9qqJqPTeGuDdHTZgpmEMUNFCq8fWCRSqJS6Jd'
        ),
        balance: 1_000_000,
      },
      {
        publicKey: PublicKey.fromBase58(
          'B62qm8mVEkhAZdnnoE4gzKhTYj2Gre88GNhoknbJdgDrE8JQojNBgWa'
        ),
        balance: 2_000_000,
      },
    ],
    100
  );

  const server = new ApolloServer<RollupContext>({
    typeDefs: await loadSchema('schema.graphql'),
    resolvers,
  });

  const { url } = await startStandaloneServer(server, {
    listen: { port: 4000 },
    context: async () => ({
      rollup: rollupState,
    }),
  });

  console.log(`🚀  Server ready at: ${url}`);
};

run().catch(console.error);
