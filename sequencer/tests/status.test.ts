import { ApolloServer } from "@apollo/server";
import assert from "assert";
import { Query, SyncStatus } from "../src/generated/graphql";
import { RollupContext, loadSchema, resolvers } from "../src/gql";

describe("Sequencer status", () => {
  let server: ApolloServer<RollupContext>;

  beforeAll(async () => {
    server = new ApolloServer<RollupContext>({
      typeDefs: await loadSchema("schema.graphql"),
      resolvers,
    });
  });

  it("returns `SYNCED` status", async () => {
    const { body } = await server.executeOperation<Pick<Query, "syncStatus">>({
      query: `
        query {
          syncStatus
        }
      `,
    });

    assert(body.kind === "single");
    expect(body.singleResult.errors).toBeUndefined();
    expect(body.singleResult.data?.syncStatus).toBe(SyncStatus.Synced);
  });

  it("returns mocked daemon status", async () => {
    const { body } = await server.executeOperation<Pick<Query, "daemonStatus">>({
      query: `
        query {
          daemonStatus {
            chainId
            syncStatus
            peers {
              host
            }
            highestBlockLengthReceived
            highestUnvalidatedBlockLengthReceived
          }
        }
      `,
    });

    assert(body.kind === "single");
    expect(body.singleResult.errors).toBeUndefined();
    expect(body.singleResult.data?.daemonStatus).toEqual({
      chainId: "69420",
      syncStatus: SyncStatus.Synced,
      peers: [],
      highestBlockLengthReceived: 0,
      highestUnvalidatedBlockLengthReceived: 0,
    });
  });
});
