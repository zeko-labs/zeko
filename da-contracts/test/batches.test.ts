import { expect } from "chai";
import { Field, Poseidon, PrivateKey, Signature, isReady as isSnarkyjsReady } from "snarkyjs";
import { MinaSchnorrSignatureStruct } from "../typechain-types/contracts/DataAvailability";
import { fieldToHex, hashPublicKey, signatureToStruct } from "../utils/mina";
import { deployDataAvailabilityContract } from "./fixtures";

describe("DataAvailability", () => {
  before(async () => {
    await isSnarkyjsReady;
  });

  it("should propose and sign batch", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey())
    );

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
      const hashedPubKey = hashPublicKey(pubKey);

      const signature = Signature.create(validator, batch);

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
