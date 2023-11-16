import fs from "fs/promises";
import { Resolvers } from "../generated/graphql";
import { mutations } from "./mutations";
import { queries } from "./queries";

export { mutations, queries };

export const resolvers: Resolvers = {
  query: queries,
  mutation: mutations,
};

export const loadSchema = async (fileName: string): Promise<string> => {
  return fs.readFile(fileName, "utf-8");
};
