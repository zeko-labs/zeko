import { expect } from "chai";
import { ethers } from "hardhat";
import { Field, PrivateKey, Signature, isReady as isSnarkyjsReady } from "snarkyjs";
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

    const ledgerHash = fieldToHex(Field.random());
    const previousLedgerHash = fieldToHex(Field(0));

    const numberOfUserCommands = randomInt(5, 10);

    const userCommands = Array.from({ length: numberOfUserCommands }, () => ({
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    }));

    const proposalTx = await dataAvailabilityContract.proposeBatch(
      ledgerHash,
      previousLedgerHash,
      userCommands
    );
    const proposalReceipt = await proposalTx.wait();

    const batchFields = bytesToFields([
      ...Buffer.from(ledgerHash.slice(2), "hex"),
      ...Buffer.from(previousLedgerHash.slice(2), "hex"),
      ...userCommands
        .map((command) => Buffer.concat([Buffer.from([command.commandType]), command.data]))
        .map((buffer) => Array.from(buffer))
        .flat(),
    ]);

    expect(
      proposalReceipt.events?.find(({ event }) => event === "BatchProposed")?.args?.ledgerHash
    ).to.equal(ledgerHash);

    const [fetchedPreviousLedgerHash, fetchedUserCommands] =
      await dataAvailabilityContract.getBatchData(ledgerHash);

    expect(fetchedPreviousLedgerHash).to.equal(previousLedgerHash);
    expect(
      fetchedUserCommands.map(({ commandType, data }) => ({
        commandType,
        data: Buffer.from(data.slice(2), "hex"),
      }))
    ).to.deep.equal(userCommands);

    const expectedSignatures: MinaSchnorrSignatureStruct[] = [];

    for (const validator of validators) {
      const pubKey = validator.toPublicKey();
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, batchFields);

      const sigStruct = signatureToStruct(signature, pubKey);

      expectedSignatures.push(sigStruct);

      const signatureTx = await dataAvailabilityContract.addBatchSignature(
        ledgerHash,
        batchFields.map(fieldToHex),
        sigStruct
      );
      const signatureReceipt = await signatureTx.wait();

      const batchSignedEvent = signatureReceipt.events?.find(
        ({ event }) => event === "BatchSigned"
      );

      expect(batchSignedEvent).to.not.be.undefined;

      if (batchSignedEvent === undefined) throw "Unreachable";

      expect(batchSignedEvent.args?.ledgerHash).to.equal(ledgerHash);
      expect(batchSignedEvent.args?.publicKey).to.equal(hashedPubKey);
      expect(batchSignedEvent.args?.signatureCount).to.equal(expectedSignatures.length);
    }

    const batchSignatures = await dataAvailabilityContract.getBatchSignatures(ledgerHash);

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

    const ledgerHash = fieldToHex(Field.random());
    const previousLedgerHash = fieldToHex(Field(0));

    const numberOfUserCommands = randomInt(5, 10);

    const userCommands = Array.from({ length: numberOfUserCommands }, () => ({
      data: ethers.utils.randomBytes(randomInt(50, 100)),
      commandType: randomInt(0, 1),
    }));

    await expect(
      dataAvailabilityContract.proposeBatch(ledgerHash, previousLedgerHash, userCommands)
    ).to.be.revertedWith("Only sequencer can call this function");
  });
});
