import { DALayer } from "./daLayer";

// this will be fetched from l1 not da layer
export const fetchCurrentLedgerHash = async (daLayer: DALayer) => {
  return await daLayer.contract.lastProposedBatch();
};
