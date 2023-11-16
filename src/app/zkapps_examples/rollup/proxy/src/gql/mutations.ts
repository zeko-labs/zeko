import { GraphQLError } from "graphql";
import { MutationResolvers } from "../generated/graphql";

export const mutations: MutationResolvers = {
  async sendPayment(_, { input, signature }) {
    throw new GraphQLError("Not implemented");
  },
};
