import { BridgeTransferResponse } from "./Services/BridgeService";

export enum SearchInputType {
  EvmAddress = "evmAddress",
  MainchainAddress = "mainchainAddress",
  MilkomedaTxId = "tx",
  MainchainTxId = "mainchainTx",
}

export type ProcessedBridgeRequestResponse = BridgeTransferResponse & {
  // symbol?: string;
  formatedValue?: string;
};

export enum TxStatus {
  Failed = "Failed",
  Pending = "Pending",
  Confirmed = "Confirmed",
  All = "All",
}

export enum FilterStatus {
  EvmAddress = "Address",
}

export type LocationState = {
  fromPathname: string;
};
