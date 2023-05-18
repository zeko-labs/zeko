import { BytesLike } from "ethers";
import { ethers } from "hardhat";
import { Field, PublicKey, Scalar, Signature } from "snarkyjs";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailabilityLayer";

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

const FIELD_BIT_CAPACITY = 254;
const FIELD_BYTE_CAPACITY = Math.floor(FIELD_BIT_CAPACITY / 8);

export const bytesToFields = (dataHex: BytesLike): Field[] => {
  const data = Array.from(ethers.utils.arrayify(dataHex));

  const fields: Field[] = [];

  for (let i = 0; i < data.length; i += FIELD_BYTE_CAPACITY) {
    const fieldBytes = data.slice(i, i + FIELD_BYTE_CAPACITY);

    while (fieldBytes.length < 32) fieldBytes.push(0);

    fields.push(Field.fromBytes(fieldBytes));
  }

  return fields;
};

export const signatureToStruct = (
  signature: Signature,
  publicKey: PublicKey
): MinaSchnorrSignatureStruct => ({
  publicKey: {
    x: fieldToHex(publicKey.toGroup().x),
    y: fieldToHex(publicKey.toGroup().y),
  },
  rx: fieldToHex(signature.r),
  s: fieldToHex(signature.s),
});

export const hashPublicKey = (publicKey: PublicKey) =>
  ethers.utils.solidityKeccak256(
    ["bytes32", "bytes32"],
    [fieldToHex(publicKey.toGroup().x), fieldToHex(publicKey.toGroup().y)]
  );
