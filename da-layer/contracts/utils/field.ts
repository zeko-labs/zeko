import { Field, Scalar } from "snarkyjs";

export const fieldToHex = (field: Field | Scalar) => {
  const bigEndianHex = BigInt(field.toJSON()).toString(16);

  const littleEndianHex =
    bigEndianHex
      .padStart(64, "0")
      .match(/.{1,2}/g)
      ?.reverse()
      .join("") ?? "";

  return `0x${littleEndianHex}`;
};
