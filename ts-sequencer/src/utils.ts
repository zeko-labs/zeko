import { Field, Scalar } from 'snarkyjs';
import { AuthRequired } from 'snarkyjs/dist/node/bindings/mina-transaction/transaction-leaves-json';
import { AccountAuthRequired } from './generated/graphql';

export const convAuthRequiredToGqlType = (authRequired: AuthRequired) => {
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

export const fieldToHex = (field: Field | Scalar) => {
  const bigEndianHex = BigInt(field.toJSON()).toString(16);

  const littleEndianHex =
    bigEndianHex
      .padStart(64, '0')
      .match(/.{1,2}/g)
      ?.reverse()
      .join('') ?? '';

  return `0x${littleEndianHex}`;
};
