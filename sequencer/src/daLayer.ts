import { ethers } from "ethers";
import { Field, Group, PublicKey, Signature } from "snarkyjs";
import { abi as DALayerAbi } from "./artifacts/DataAvailability.json";
import config from "./config";
import { Batch } from "./rollup";
import { DataAvailability } from "./typechain-types";
import { MinaCommandStruct } from "./typechain-types/contracts/DataAvailability";

export const provider = new ethers.providers.WebSocketProvider(config.DA_LAYER_WEBSOCKET_URL);

export const wallet = new ethers.Wallet(config.DA_LAYER_PRIVATE_KEY, provider);

export const daLayerContract = new ethers.Contract(
  config.DA_LAYER_CONTRACT_ADDRESS,
  DALayerAbi,
  wallet
) as DataAvailability;

export const fetchBatches = async (ledgerHash: string) => {
  let currentLedgerHash = ledgerHash;
  let previousLedgerHash;
  let transactions;

  const reversedBatches: Batch[] = [];

  while (currentLedgerHash !== ethers.constants.HashZero) {
    [previousLedgerHash, transactions] = await daLayerContract.getBatchData(currentLedgerHash);

    const batch = {
      ledgerHash: currentLedgerHash,
      transactions: transactions.map((tx) => ({
        id: Buffer.from(tx.data.slice(2), "hex").toString("base64"),
        commandType: tx.commandType,
      })),
    };

    reversedBatches.push(batch);

    currentLedgerHash = previousLedgerHash;
  }

  return reversedBatches.reverse();
};

export const postBatch = async (batch: Batch, previousLedgerHash: string) => {
  const proposedCommands: MinaCommandStruct[] = batch.transactions.map(({ id, commandType }) => ({
    commandType,
    data: Buffer.from(id, "base64"),
  }));

  const tx = await daLayerContract.proposeBatch(batch.ledgerHash, previousLedgerHash, proposedCommands);

  await tx.wait();

  console.log("Batch posted: ", batch.ledgerHash);

  const quorum = await daLayerContract.quorum();

  return new Promise<
    {
      publicKey: PublicKey;
      signature: Signature;
    }[]
  >((resolve) => {
    daLayerContract.on("BatchSigned", async (signedLedgerHash, _, signatureCount) => {
      if (signatureCount < quorum || signedLedgerHash !== batch.ledgerHash) return;

      const solSignatures = await daLayerContract.getBatchSignatures(batch.ledgerHash);

      const signatures = solSignatures.map((sig) => {
        const pubKeyGroup = Group.fromJSON({
          x: Field.fromBytes(Array.from(Buffer.from(sig.publicKey.x.slice(2), "hex"))).toString(),
          y: Field.fromBytes(Array.from(Buffer.from(sig.publicKey.y.slice(2), "hex"))).toString(),
        });

        if (pubKeyGroup === null) throw new Error("Invalid public key");

        const publicKey = PublicKey.fromGroup(pubKeyGroup);

        const signature = Signature.fromJSON({
          r: Field.fromBytes(Array.from(Buffer.from(sig.rx.slice(2), "hex"))).toString(),
          s: Field.fromBytes(Array.from(Buffer.from(sig.s.slice(2), "hex"))).toString(),
        });

        return {
          publicKey,
          signature,
        };
      });

      resolve(signatures);
    });
  });
};
