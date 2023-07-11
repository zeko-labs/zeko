import { daLayerContract } from "./daLayer";

// this will be fetched from l1 not da layer
export const fetchCurrentLedgerHash = async () => {
  return await daLayerContract.lastProposedBatch();
};
