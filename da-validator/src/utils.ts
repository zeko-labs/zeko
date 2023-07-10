import { Field, PublicKey, Scalar, Signature } from "snarkyjs";
import { MinaSchnorrSignatureStruct } from "./typechain-types/contracts/DataAvailability";

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

export const hexToField = (hex: string): Field => {
  const littleEndianHex =
    hex
      .slice(2)
      .match(/.{1,2}/g)
      ?.reverse()
      .join("") ?? "";

  return Field(BigInt(`0x${littleEndianHex}`));
};

export const signatureToStruct = (signature: Signature, publicKey: PublicKey): MinaSchnorrSignatureStruct => ({
  publicKey: {
    x: fieldToHex(publicKey.toGroup().x),
    y: fieldToHex(publicKey.toGroup().y),
  },
  rx: fieldToHex(signature.r),
  s: fieldToHex(signature.s),
});
