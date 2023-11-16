import { expect } from "chai";
import { ethers } from "hardhat";
import { Field, PrivateKey, Signature, isReady as isSnarkyjsReady } from "snarkyjs";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailability";
import { fieldToHex, hashPublicKey, signatureToStruct } from "../utils/mina";
import { deployDataAvailabilityContract } from "./fixtures";

function randomInt(min: number, max: number) {
  min = Math.ceil(min);
  max = Math.floor(max);

  return Math.floor(Math.random() * (max - min) + min);
}

describe("Commands DataAvailability", () => {
  before(async () => {
    await isSnarkyjsReady;
  });

  it("should propose and sign command", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey())
    );

    const expectedIndex = 0;
    const commandCommitment = Field.random();
    const command = {
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    };

    const proposalTx = await dataAvailabilityContract.postCommand(command);
    const postingReceipt = await proposalTx.wait();

    expect(
      postingReceipt.events?.find(({ event }) => event === "CommandPosted")?.args?.index
    ).to.equal(expectedIndex);

    const { commandType, data } = await dataAvailabilityContract.getCommandData(expectedIndex);

    expect({
      commandType,
      data: Buffer.from(data.slice(2), "hex"),
    }).to.deep.equal(command);

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, [commandCommitment]);

      const sigStruct = signatureToStruct(signature, pubKey);

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.addCommandSignature(
        expectedIndex,
        fieldToHex(commandCommitment),
        sigStruct
      );
      const signatureReceipt = await signatureTx.wait();

      const commandSignedEvent = signatureReceipt.events?.find(
        ({ event }) => event === "CommandSigned"
      );

      expect(commandSignedEvent).to.not.be.undefined;

      if (commandSignedEvent === undefined) throw "Unreachable";

      expect(commandSignedEvent.args?.index).to.equal(expectedIndex);
      expect(commandSignedEvent.args?.publicKey).to.equal(hashedPubKey);
      expect(commandSignedEvent.args?.signatureCount).to.equal(expectedSignatures.length);
    }

    const commandSignatures = await dataAvailabilityContract.getCommandSignatures(expectedIndex);

    expect(
      commandSignatures.map(({ publicKey, rx, s }) => ({
        publicKey: {
          x: publicKey.x,
          y: publicKey.y,
        },
        rx,
        s,
      }))
    ).to.deep.equal(expectedSignatures);
  });

  it("should fail if the command poster is not sequencer", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      5,
      ethers.Wallet.createRandom().address
    );

    const command = {
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    };

    await expect(dataAvailabilityContract.postCommand(command)).to.be.revertedWith(
      "Only sequencer can call this function"
    );
  });
});
