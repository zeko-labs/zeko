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

    const genesisState = ethers.utils.randomBytes(randomInt(50, 100));
    await dataAvailabilityContract.initGenesisState(genesisState).then((tx) => tx.wait());

    const batchData = ethers.utils.randomBytes(randomInt(50, 100)).toString();
    const sigData = Array.from({ length: randomInt(5, 10) }, () => Field.random());

    const proposalTx = await dataAvailabilityContract.postBatch(
      batchData,
      sigData.map(fieldToHex)
    );
    const proposalReceipt = await proposalTx.wait();

    const expectedLocation = (await dataAvailabilityContract.batchesLength()).sub(1);

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchPosted")?.args?.location
    ).to.equal(expectedLocation);

    const [fetchedBatchData, fetchedSigData] = await dataAvailabilityContract.getBatchData(
      expectedLocation
    );

    expect(fetchedBatchData).to.deep.equal(batchData);
    expect(fetchedSigData).to.deep.equal(sigData.map(fieldToHex));

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

    console.log(dataAvailabilityContract.address);
    console.log(expectedLocation.toString());
  });

  it("should fail if the proposer is not sequencer", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const [fundedAccount] = await ethers.getSigners();
    const sequencer = ethers.Wallet.createRandom().connect(ethers.provider);

    await fundedAccount
      .sendTransaction({
        to: sequencer.address,
        value: ethers.utils.parseEther("1"),
      })
      .then((tx) => tx.wait());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      5,
      sequencer.address
    );

    const genesisState = ethers.utils.randomBytes(randomInt(50, 100));
    await dataAvailabilityContract
      .connect(sequencer)
      .initGenesisState(genesisState)
      .then((tx) => tx.wait());

    const batchData = ethers.utils.randomBytes(randomInt(50, 100)).toString();
    const sigData = Array.from({ length: randomInt(5, 10) }, () => Field.random());

    await expect(
      dataAvailabilityContract.postBatch(batchData, sigData.map(fieldToHex))
    ).to.be.revertedWith("Only sequencer can call this function");
  });
});
