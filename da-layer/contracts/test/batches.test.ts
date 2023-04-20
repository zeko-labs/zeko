import { expect } from "chai";
import { ethers } from "hardhat";
import { Field, Poseidon, PrivateKey, Signature, isReady as isSnarkyjsReady } from "snarkyjs";
import { DataAvailabilityLayer } from "../typechain-types";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailabilityLayer";
import { fieldToHex } from "../utils/field";
import { deployDataAvailabilityContract } from "./fixtures";

describe("DataAvailability", () => {
  let dataAvailabilityContract: DataAvailabilityLayer;
  let validators: PrivateKey[];

  before(async () => {
    await isSnarkyjsReady;

    validators = Array.from({ length: 5 }, () => PrivateKey.random());

    dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey())
    );
  });

  it("should add signatures", async () => {
    const batch = Array.from({ length: 32 }, () => Field.random());

    const batchId = fieldToHex(Poseidon.hash(batch));

    const proposalTx = await dataAvailabilityContract.proposeBatch(batch.map(fieldToHex));
    const proposalReceipt = await proposalTx.wait();

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchProposed")?.args?.batchId
    ).to.equal(batchId);

    expect(await dataAvailabilityContract.getBatchData(batchId)).to.deep.equal(
      batch.map(fieldToHex)
    );

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = ethers.utils.solidityKeccak256(
        ["bytes32", "bytes32"],
        [fieldToHex(pubKey.toGroup().x), fieldToHex(pubKey.toGroup().y)]
      );

      const signature = Signature.create(validator, batch);

      const sigStruct = {
        publicKey: {
          x: fieldToHex(pubKey.toGroup().x),
          y: fieldToHex(pubKey.toGroup().y),
        },
        rx: fieldToHex(signature.r),
        s: fieldToHex(signature.s),
      };

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.signBatch(batchId, sigStruct);
      const signatureReceipt = await signatureTx.wait();

      expect(
        signatureReceipt.events?.find(({ event }) => event === "BatchSigned")?.args?.batchId
      ).to.equal(batchId);
      expect(
        signatureReceipt.events?.find(({ event }) => event === "BatchSigned")?.args?.publicKey
      ).to.equal(hashedPubKey);
      expect(
        signatureReceipt.events?.find(({ event }) => event === "BatchSigned")?.args?.signatureCount
      ).to.equal(expectedSignatures.length);
    }

    const batchSignatures = await dataAvailabilityContract.getBatchSignatures(batchId);

    expect(
      batchSignatures.map(({ publicKey, rx, s }) => ({
        publicKey: {
          x: publicKey.x,
          y: publicKey.y,
        },
        rx,
        s,
      }))
    ).to.deep.equal(expectedSignatures);
  });
});
