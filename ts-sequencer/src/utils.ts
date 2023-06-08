import { AuthRequired } from 'snarkyjs/dist/node/bindings/mina-transaction/transaction-leaves-json';
import { AccountAuthRequired } from './generated/graphql';

export const authRequiredToGql = (authRequired: AuthRequired) => {
  switch (authRequired) {
    case 'Either':
      return AccountAuthRequired.Either;
    case 'Impossible':
      return AccountAuthRequired.Impossible;
    case 'None':
      return AccountAuthRequired.None;
    case 'Proof':
      return AccountAuthRequired.Proof;
    case 'Signature':
      return AccountAuthRequired.Signature;
  }
};
