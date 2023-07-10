import { Ledger, Poseidon, PrivateKey, Signature } from "snarkyjs";
import config from "./config";
import { daLayerContract } from "./daLayer";
import { MinaSchnorrSignatureStruct } from "./typechain-types/contracts/DataAvailability";
import { fieldToHex, hexToField, signatureToStruct } from "./utils";

const minaPrivateKey = PrivateKey.fromBase58(config.MINA_SIGNING_KEY);

export enum CommandType {
  SignedCommand = 0,
  ZkappCommand = 1,
}

const postSignature = async (ledgerHash: string, batchCommitment: string, signature: MinaSchnorrSignatureStruct) => {
  const tx = await daLayerContract.addBatchSignature(ledgerHash, [batchCommitment], signature);

  await tx.wait();
};

const signBatch = async (
  privateKey: PrivateKey,
  previousLedgerHash: string,
  commands: { commandType: CommandType; data: string }[]
) => {
  const commitments = [
    hexToField(previousLedgerHash),
    ...commands.map(({ commandType, data }) => {
      switch (commandType) {
        case CommandType.SignedCommand:
          const fields = Ledger.paymentInput(Buffer.from(data.slice(2), "hex").toString("base64"));
          return Poseidon.hash(fields);

        case CommandType.ZkappCommand:
          return Ledger.transactionCommitments(
            Ledger.encoding.jsonZkappCommandFromBase64(Buffer.from(data.slice(2), "hex").toString("base64"))
          ).fullCommitment;

        default:
          throw new Error(`Unknown command type: ${commandType}`);
      }
    }),
  ];

  const batchCommitment = Poseidon.hash(commitments);

  const signature = Signature.create(privateKey, [batchCommitment]);

  return { signature, batchCommitment };
};

const processBatch = async (ledgerHash: string) => {
  const [quorum, signatures] = await Promise.all([
    daLayerContract.quorum(),
    daLayerContract.getBatchSignatures(ledgerHash),
  ]);

  if (
    signatures.length >= quorum.toNumber() ||
    signatures.some(({ publicKey }) => {
      const { x, y } = minaPrivateKey.toPublicKey().toGroup();

      return fieldToHex(x) === publicKey.x && fieldToHex(y) === publicKey.y;
    })
  ) {
    console.log("Skipping batch", ledgerHash);
    return;
  }

  const [previousLedgerHash, commands] = await daLayerContract.getBatchData(ledgerHash);

  const { signature, batchCommitment } = await signBatch(minaPrivateKey, previousLedgerHash, commands);

  await postSignature(
    ledgerHash,
    fieldToHex(batchCommitment),
    signatureToStruct(signature, minaPrivateKey.toPublicKey())
  );

  console.log("Batch signed", ledgerHash);
};

const run = async () => {
  const previousBatches = await daLayerContract.queryFilter(daLayerContract.filters.BatchProposed(), config.FROM_BLOCK);

  for (const batch of previousBatches) {
    const currentLedgerHash = batch.args?.ledgerHash;

    await processBatch(currentLedgerHash);
  }

  daLayerContract.on("BatchProposed", async (ledgerHash) => {
    console.log("BatchProposed", ledgerHash);

    await processBatch(ledgerHash);
  });
};

run().catch(console.error);
