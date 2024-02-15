import Cip5 from "@dcspark/cip5-js";
import { bech32 } from "bech32";
import BigNumber from "bignumber.js";
import { ethers } from "ethers";
import { MAIN_ASSET_ID } from "../Constants";
import moment from "moment";
import { Network } from "./ConfigService";

export type AssetDataMapItem = {
  asset_id: string;
  decimals: number;
  symbol: string;
};

export enum RequestType {
  WRAP = "WRAP",
  UNWRAP = "UNWRAP",
}

class UtilService {
  public static informAboutUnwrappingConfirmation = (
    mainchain: string,
    confirmationTime: number | null,
    invalidated?: boolean
  ) => {
    if (invalidated !== undefined && invalidated === true) {
      return "Request failed to be processed.";
    }

    return confirmationTime !== null
      ? this.formatTime(confirmationTime)
      : `awaiting confirmation on ${mainchain} side`;
  };

  public static informAboutWrappingConfirmation = (
    confirmationTime: number | null,
    invalidated?: boolean
  ) => {
    if (invalidated !== undefined && invalidated === true) {
      return "Request failed to be processed.";
    }

    return confirmationTime !== null
      ? this.formatTime(confirmationTime)
      : "awaiting confirmation on EVM side";
  };

  public static formatTime(confirmationTime: number): string {
    return `${moment.unix(confirmationTime).format("mm")} min ${moment
      .unix(confirmationTime)
      .format("ss")} s`;
  }

  public static timestampToDateTime(timestamp: number): string {
    return new Date(timestamp * 1000).toLocaleString(undefined, {
      dateStyle: "long",
      timeStyle: "long",
    });
  }

  public static getExplorerFromTxUrlBase(selectedNetwork: Network, direction: RequestType): string {
    return direction === RequestType.WRAP
      ? `${selectedNetwork.mainchainExplorerBaseURL}/transaction`
      : `${selectedNetwork.rollupExplorerBaseURL}/tx`;
  }

  public static getExplorerToTxUrlBase(selectedNetwork: Network, direction: RequestType): string {
    return direction === RequestType.WRAP
      ? `${selectedNetwork.rollupExplorerBaseURL}/tx`
      : `${selectedNetwork.mainchainExplorerBaseURL}/transaction`;
  }

  public static formatCompactNumber(value: string): string | number {
    try {
      const formatter = Intl.NumberFormat("en", { notation: "compact" });
      return formatter.format(value as unknown as number);
    } catch (err) {
      const error = err as Error;
      console.error(error.message);
      return value;
    }
  }

  public static formatValue(
    value: string,
    assetId: string,
    assetDataMap?: Record<string, AssetDataMapItem>,
    mainDecimals?: number,
    mainchainName?: string
  ) {
    let assetAmount = "0";
    if (assetId === MAIN_ASSET_ID) {
      assetAmount = this.decimalsToMainUnitConversion(value, mainDecimals);
      return assetAmount;
    }

    try {
      if (assetDataMap != null && assetDataMap[assetId] != null)
        return ethers.utils.formatUnits(value, assetDataMap[assetId].decimals);
      else return value;
    } catch (e) {
      return value;
    }
  }

  public static decimalsToMainUnitConversion(mainValue: string, mainDecimals?: number): string {
    if (mainDecimals === 0) return mainValue;
    if (mainDecimals !== undefined) {
      return new BigNumber(mainValue).div(new BigNumber("10").pow(mainDecimals)).toString();
    }
    return new BigNumber(mainValue).div(new BigNumber("1000000000000000000")).toString();
  }

  public static isEvmTxIdAsync = (txId: string) => {
    // try basic regex, to just validate if tx is valid evm tx id
    return /^0x([A-Fa-f0-9]{64})$/.test(txId);
  };

  public static isCardanoAddress = (address: string) => {
    try {
      // Not sure what should be the limit for Cardano Addresses?
      // Also not sure if I should use serialization-lib vs just bech32 decoding
      // for detecing if its a valid cardano address
      const bech32Info = bech32.decode(address, 1000);
      if (bech32Info.prefix === Cip5.miscellaneous.addr) {
        return true;
      } else {
        return false;
      }
    } catch (e) {
      return false;
    }
  };

  public static truncateString = (value: string): string => {
    return `${value.substring(0, 40)}..${value.substring(value.length - 12, value.length)}`;
  };

  public static isValidStr = (value: string | undefined | null): boolean => {
    return !(!value || value.length === 0);
  };
}

export { UtilService };
