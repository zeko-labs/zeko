import config from "../_config/config.json";

export type Network = {
  id: string;
  name: string;
  mainchain: string;
  wrapAssetSymbol: string;
  unwrapAssetSymbol: string;
  hideIcon?: boolean;
  bridgeApiBaseURL: string;
  rollupExplorerBaseURL: string;
  mainchainExplorerBaseURL: string;
  mainAssetDecimals: number;
  mainchainDetais?: {
    nodeUrl: string;
    apiKey: string;
  };
};

type ConfigMainValues = {
  bridgeApiBaseURL: string;
  rollupExplorerBaseURL: string;
  mainchainExplorerBaseURL: string;
};

export enum QueryKeys {
  COMPLETED_TRANSFERS = "COMPLETED_TRANSFERS",
  PENDING_TRANSFERS = "PENDING_TRANSFERS",
  TRANSFER_DETAIL = "TRANSFER_DETAIL",
  BRIDGE_STATS = "BRIDGE_STATS",
  SEARCH = "SEARCH",
}

export enum Networks {
  mainnet = "mainnet",
  devnet = "devnet",
}

const c1Mainnet = config["c1-mainnet"] as ConfigMainValues;
const c1Devnet = config["c1-devnet"] as ConfigMainValues;

const networksMap: Record<Networks, Network> = {
  [Networks.mainnet]: {
    id: Networks.mainnet,
    name: "Mina Mainnet",
    mainchain: "Cardano",
    wrapAssetSymbol: "ADA",
    unwrapAssetSymbol: "MADA",
    bridgeApiBaseURL: c1Mainnet.bridgeApiBaseURL ?? "",
    rollupExplorerBaseURL: c1Mainnet.rollupExplorerBaseURL ?? "",
    mainchainExplorerBaseURL: c1Mainnet.mainchainExplorerBaseURL ?? "",
    mainAssetDecimals: 18,
  },
  [Networks.devnet]: {
    id: Networks.devnet,
    name: "Mina Devnet",
    mainchain: "Cardano",
    wrapAssetSymbol: "ADA",
    unwrapAssetSymbol: "MADA",
    hideIcon: true,
    bridgeApiBaseURL: c1Devnet.bridgeApiBaseURL ?? "",
    rollupExplorerBaseURL: c1Devnet.rollupExplorerBaseURL ?? "",
    mainchainExplorerBaseURL: c1Devnet.mainchainExplorerBaseURL ?? "",
    mainAssetDecimals: 18,
  },
};

export { networksMap };
