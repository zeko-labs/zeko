import { AuthRequired } from 'snarkyjs/dist/node/bindings/mina-transaction/transaction-leaves-json';
import { MlBytes } from 'snarkyjs/dist/node/snarky';
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

export const MlBytesToBigInt = (x: MlBytes): bigint => {
  const buffer = Buffer.from(x.c, 'binary');
  return BigInt(buffer.reverse().toString('hex'));
};

export const BigIntToMlBytes = (x: bigint): MlBytes => {
  const buffer = Buffer.from(x.toString(16), 'hex');

  return {
    t: 0,
    c: buffer.reverse().toString('binary'),
    l: buffer.length,
  };
};
