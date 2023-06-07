import { GraphQLError } from 'graphql';
import { MutationResolvers, SendZkappPayload } from '../generated/graphql';
import { RollupContext } from '../rollup';

export const mutations: MutationResolvers = {
  sendZkapp(_, { input }, { rollup }: RollupContext): SendZkappPayload {
    try {
      rollup.ledger.applyJsonTransaction(
        JSON.stringify(input.zkappCommand),
        rollup.networkConstants.accountCreationFee.toString(),
        JSON.stringify(rollup.networkState)
      );
    } catch (e) {
      if (e instanceof Error) {
        throw new GraphQLError(e.message);
      }
      throw new GraphQLError('Unknown error');
    }

    return {
      zkapp: {
        failureReason: null,
        hash: 'TODO',
        id: 'TODO',
        zkappCommand: input.zkappCommand,
      },
    };
  },
};
