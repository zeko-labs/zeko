import { ethers } from "ethers";
import { abi as DALayerAbi } from "./artifacts/DataAvailability.json";
import config from "./config";
import { DataAvailability } from "./typechain-types";

export const provider = new ethers.providers.WebSocketProvider(config.DA_LAYER_WEBSOCKET_URL);

export const wallet = new ethers.Wallet(config.DA_LAYER_PRIVATE_KEY, provider);

export const daLayerContract = new ethers.Contract(
  config.DA_LAYER_CONTRACT_ADDRESS,
  DALayerAbi,
  wallet
) as DataAvailability;
