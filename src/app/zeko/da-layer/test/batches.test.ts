import { expect } from "chai";
import { ethers } from "hardhat";
import { Field, PrivateKey, Signature } from "o1js";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailability";
import { fieldToHex, hashPublicKey, signatureToStruct } from "../utils/mina";
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

    const previousLocation = -1;

    const numberOfUserCommands = randomInt(5, 10);

    const sourceReceiptChainHashes = ethers.utils.randomBytes(randomInt(50, 100));
    const userCommands = Array.from({ length: numberOfUserCommands }, () =>
      ethers.utils.randomBytes(randomInt(50, 100))
    );
    const targetSparseLedger = ethers.utils.randomBytes(randomInt(50, 100));
    const sigData = Array.from({ length: randomInt(5, 10) }, () => Field.random());

    const proposalTx = await dataAvailabilityContract.postBatch(
      previousLocation,
      sourceReceiptChainHashes,
      userCommands,
      targetSparseLedger,
      sigData.map(fieldToHex)
    );
    const proposalReceipt = await proposalTx.wait();

    const expectedLocation = (await dataAvailabilityContract.batchesLength()).sub(1);

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchPosted")?.args?.location
    ).to.equal(expectedLocation);

    const [fetchedPreviousLocation] = await dataAvailabilityContract.getBatchData(
      expectedLocation
    );

    expect(fetchedPreviousLocation).to.equal(previousLocation);

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, sigData);

      const sigStruct = signatureToStruct(signature, pubKey);

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.addBatchSignature(
        expectedLocation,
        sigStruct
      );
      const signatureReceipt = await signatureTx.wait();

      const batchSignedEvent = signatureReceipt.events?.find(
        ({ event }) => event === "BatchSigned"
      );

      expect(batchSignedEvent).to.not.be.undefined;

      if (batchSignedEvent === undefined) throw "Unreachable";

      expect(batchSignedEvent.args?.location).to.equal(expectedLocation);
      expect(batchSignedEvent.args?.publicKey).to.equal(hashedPubKey);
      expect(batchSignedEvent.args?.signatureCount).to.equal(expectedSignatures.length);
    }

    const batchSignatures = await dataAvailabilityContract.getBatchSignatures(expectedLocation);

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

    const previousLocation = -1;

    const numberOfUserCommands = randomInt(5, 10);

    const sourceReceiptChainHashes = ethers.utils.randomBytes(randomInt(50, 100));
    const userCommands = Array.from({ length: numberOfUserCommands }, () =>
      ethers.utils.randomBytes(randomInt(50, 100))
    );
    const targetSparseLedger = ethers.utils.randomBytes(randomInt(50, 100));
    const sigData = Array.from({ length: randomInt(5, 10) }, () => Field.random());

    await expect(
      dataAvailabilityContract.postBatch(
        previousLocation,
        sourceReceiptChainHashes,
        userCommands,
        targetSparseLedger,
        sigData.map(fieldToHex)
      )
    ).to.be.revertedWith("Only sequencer can call this function");
  });
});
