import { GraphQLError } from 'graphql';
import { PublicKey, Signature } from 'snarkyjs';
import { MutationResolvers, SendZkappPayload } from '../generated/graphql';
import { RollupContext } from '../rollup';
import { queries } from './queries';

export const mutations: MutationResolvers = {
  sendZkapp(_, { input }, { rollup }: RollupContext): SendZkappPayload {
    try {
      const { hash, id } = rollup.ledger.applyJsonTransaction(
        JSON.stringify(input.zkappCommand),
        rollup.networkConstants.accountCreationFee.toString(),
        JSON.stringify(rollup.networkState)
      );

      return {
        zkapp: {
          failureReason: null,
          hash,
          id,
          zkappCommand: input.zkappCommand,
        },
      };
    } catch (e) {
      if (e instanceof Error) {
        throw new GraphQLError(e.message);
      }
      throw new GraphQLError('Unknown error');
    }
  },

  sendPayment(_, { input, signature }, { rollup }: RollupContext) {
    try {
      const { amount, fee, from, memo, nonce, to, validUntil } = input;

      const sigBase58 = Signature.fromJSON({
        r: signature?.field,
        s: signature?.scalar,
      }).toBase58();

      const { hash, id } = rollup.ledger.applyPayment(
        sigBase58,
        PublicKey.fromBase58(from),
        PublicKey.fromBase58(to),
        amount.toString(),
        fee.toString(),
        validUntil.toString(),
        nonce.toString(),
        memo?.toString() ?? '',
        rollup.networkConstants.accountCreationFee.toString(),
        JSON.stringify(rollup.networkState)
      );

      // @ts-expect-error
      const feePayer = queries.account(null, { publicKey: from }, { rollup });

      // @ts-expect-error
      const receiver = queries.account(null, { publicKey: to }, { rollup });

      return {
        payment: {
          __typename: 'UserCommandPayment',
          amount: input.amount,
          failureReason: null,
          fee: input.fee,
          feePayer,
          feeToken: '1',
          from: input.from,
          fromAccount: feePayer,
          hash: hash,
          id,
          isDelegation: false,
          kind: 'Payment',
          memo: input.memo ?? '',
          nonce: input.nonce,
          receiver,
          source: feePayer,
          to: input.to,
          toAccount: receiver,
          token: '1',
          validUntil: input.validUntil,
        },
      };
    } catch (e) {
      console.error(e);
      if (e instanceof Error) {
        throw new GraphQLError(e.message);
      }
      throw new GraphQLError('Unknown error');
    }
  },
};
