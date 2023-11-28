import { expect } from "chai";
import { ethers } from "hardhat";
import { Field, PrivateKey, Signature } from "o1js";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailability";
import { fieldToHex, hashPublicKey, hexToField, signatureToStruct } from "../utils/mina";
import { deployDataAvailabilityContract } from "./fixtures";

function randomInt(min: number, max: number) {
  min = Math.ceil(min);
  max = Math.floor(max);

  return Math.floor(Math.random() * (max - min) + min);
}

describe("Batches DataAvailability", () => {
  it("should propose and sign batch", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey())
    );

    const batchIdField = Field.random();
    const batchId = fieldToHex(batchIdField);
    const previousId = fieldToHex(Field(0));

    expect(hexToField(batchId)).to.deep.equal(batchIdField);

    const numberOfUserCommands = randomInt(5, 10);

    const userCommands = Array.from({ length: numberOfUserCommands }, () => ({
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    }));

    const proposalTx = await dataAvailabilityContract.postBatch(batchId, previousId, userCommands);
    const proposalReceipt = await proposalTx.wait();

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchPosted")?.args?.id
    ).to.equal(batchId);

    const [genesis, fetchedPreviousBatchId, fetchedUserCommands] =
      await dataAvailabilityContract.getBatchData(batchId);

    expect(genesis).to.be.true;

    expect(fetchedPreviousBatchId).to.equal(previousId);

    expect(
      fetchedUserCommands.map(({ data, commandType }) => ({
        data: ethers.utils.arrayify(data),
        commandType,
      }))
    ).to.deep.equal(userCommands);

    expect(await dataAvailabilityContract.lastBatchId()).to.equal(batchId);

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, [batchIdField]);

      const sigStruct = signatureToStruct(signature, pubKey);

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.addBatchSignature(batchId, sigStruct);
      const signatureReceipt = await signatureTx.wait();

      const batchSignedEvent = signatureReceipt.events?.find(
        ({ event }) => event === "BatchSigned"
      );

      expect(batchSignedEvent).to.not.be.undefined;

      if (batchSignedEvent === undefined) throw "Unreachable";

      expect(batchSignedEvent.args?.id).to.equal(batchId);
      expect(batchSignedEvent.args?.publicKey).to.equal(hashedPubKey);
      expect(batchSignedEvent.args?.signatureCount).to.equal(expectedSignatures.length);
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

  it("should fail if the proposer is not sequencer", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      5,
      ethers.Wallet.createRandom().address
    );

    const batchId = fieldToHex(Field.random());
    const previousId = fieldToHex(Field(0));

    const numberOfUserCommands = randomInt(5, 10);

    const userCommands = Array.from({ length: numberOfUserCommands }, () => ({
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    }));

    await expect(
      dataAvailabilityContract.postBatch(batchId, previousId, userCommands)
    ).to.be.revertedWith("Only sequencer can call this function");
  });
});
