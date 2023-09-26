import { ethers } from "ethers";
import { abi as DALayerAbi } from "./artifacts/DataAvailability.json";
import config from "./config";
import logger from "./logger";
import { Transaction } from "./rollup";
import { DataAvailability } from "./typechain-types";
import { CommandPostedEvent } from "./typechain-types/contracts/DataAvailability";

export class DALayer {
  private readonly provider = new ethers.providers.WebSocketProvider(config.DA_LAYER_WEBSOCKET_URL);

  private readonly wallet = new ethers.Wallet(config.DA_LAYER_PRIVATE_KEY, this.provider);

  public readonly contract = new ethers.Contract(
    config.DA_LAYER_CONTRACT_ADDRESS,
    DALayerAbi,
    this.wallet
  ) as DataAvailability;

  private pingTimeout: NodeJS.Timeout | null = null;
  private keepAliveInterval: NodeJS.Timer | null = null;

  constructor() {
    this.keepAlive();
  }

  public stop() {
    this.clearKeepAliveInterval();
    this.clearPingTimeout();
    this.provider._websocket.terminate();
  }

  public async postCommand(command: Transaction) {
    const tx = await this.contract.postCommand({
      commandType: command.commandType,
      data: Buffer.from(command.id, "base64"),
    });

    const receipt = await tx.wait();

    const event = receipt.events?.find((e) => e.event === "CommandPosted") as CommandPostedEvent | undefined;

    if (event === undefined) {
      throw new Error("Command posting failed");
    }

    const commandIndex = event.args.index.toNumber();

    logger.info(`Command posted with id: ${commandIndex}`);

    return commandIndex;
  }

  private keepAlive() {
    this.provider._websocket.on("open", () => {
      this.keepAliveInterval = setInterval(() => {
        this.provider._websocket.ping();

        this.pingTimeout = setTimeout(() => {
          this.provider._websocket.terminate();
        }, config.WS_EXPECTED_PONG_BACK);
      }, config.WS_KEEP_ALIVE_INTERVAL);
    });

    this.provider._websocket.on("close", () => {
      this.clearKeepAliveInterval();
      this.clearPingTimeout();
      this.keepAlive();
    });

    this.provider._websocket.on("pong", () => {
      this.clearPingTimeout();
    });
  }

  private clearPingTimeout() {
    if (this.pingTimeout) {
      clearInterval(this.pingTimeout);
    }
  }

  private clearKeepAliveInterval() {
    if (this.keepAliveInterval) {
      clearInterval(this.keepAliveInterval);
    }
  }
}
