import { expect } from "chai";
import { ethers } from "hardhat";
import { Poseidon, PrivateKey, Signature, isReady as isSnarkyjsReady } from "snarkyjs";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailability";
import { bytesToFields, fieldToHex, hashPublicKey, signatureToStruct } from "../utils/mina";
import { deployDataAvailabilityContract } from "./fixtures";

function randomInt(min: number, max: number) {
  min = Math.ceil(min);
  max = Math.floor(max);

  return Math.floor(Math.random() * (max - min) + min);
}

describe("DataAvailability", () => {
  before(async () => {
    await isSnarkyjsReady;
  });

  it("should propose and sign batch", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey())
    );

    const previousBatchId = ethers.utils.randomBytes(32);

    const numberOfUserCommands = randomInt(5, 10);

    const userCommands = Array.from({ length: numberOfUserCommands }, () => ({
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    }));

    const proposalTx = await dataAvailabilityContract.proposeBatch(previousBatchId, userCommands);
    const proposalReceipt = await proposalTx.wait();

    const batchFields = bytesToFields([
      ...previousBatchId,
      ...userCommands
        .map((command) => Buffer.concat([Buffer.from([command.commandType]), command.data]))
        .map((buffer) => Array.from(buffer))
        .flat(),
    ]);

    const batchId = fieldToHex(Poseidon.hash(batchFields));

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchProposed")?.args?.batchId
    ).to.equal(batchId);

    const [fetchedPreviousBatchId, fetchedUserCommands] =
      await dataAvailabilityContract.getBatchData(batchId);

    expect(fetchedPreviousBatchId).to.equal(`0x${Buffer.from(previousBatchId).toString("hex")}`);

    expect(
      fetchedUserCommands.map(({ commandType, data }) => ({
        commandType,
        data: Buffer.from(data.slice(2), "hex"),
      }))
    ).to.deep.equal(userCommands);

    expect(await dataAvailabilityContract.getBatchFields(batchId)).to.deep.equal(
      batchFields.map(fieldToHex)
    );

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, batchFields);

      const sigStruct = signatureToStruct(signature, pubKey);

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.addBatchSignature(batchId, sigStruct);
      const signatureReceipt = await signatureTx.wait();

      const batchSignedEvent = signatureReceipt.events?.find(
        ({ event }) => event === "BatchSigned"
      );

      expect(batchSignedEvent).to.not.be.undefined;

      if (batchSignedEvent === undefined) throw "Unreachable";

      expect(batchSignedEvent.args?.batchId).to.equal(batchId);
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
});
