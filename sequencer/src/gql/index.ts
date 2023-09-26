import fs from "fs/promises";
import { Resolvers } from "../generated/graphql";
import { Rollup } from "../rollup";
import { mutations } from "./mutations";
import { queries } from "./queries";

export { mutations, queries };

export type RollupContext = {
  rollup: Rollup;
  teardown: () => Promise<void>;
};

export const resolvers: Resolvers = {
  query: queries,
  mutation: mutations,
};

export const loadSchema = async (fileName: string): Promise<string> => {
  return fs.readFile(fileName, "utf-8");
};
