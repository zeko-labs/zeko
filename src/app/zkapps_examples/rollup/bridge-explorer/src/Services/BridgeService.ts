// import Config from "../Config";
import { TxStatus } from "../Types";
import { generateUrlWithQueryParams } from "../utils";
import { RequestType } from "./UtilService";

export type BridgeUnwrappingsResponse = {
  request_id: string;
  mainchain_tx_id: string;
  transaction_id: string;
  sidechain_address: string;
  mainchain_address: string;
  creator: string;
  block_number: number;
  block_id: string;
  confirmed_block_number: number | null;
  executed_block_number: number | null;
  confirmed_block_id: string | null;
  executed_block_id: string | null;
  assets: [
    {
      request_id: string;
      asset_id: string;
      asset_value: string;
    }
  ];
};

export type BridgeWrappingResponse = {
  row_id: number;
  mainchain_tx_id: string;
  to: string;
  block_number: number;
  block_id: string;
  stargate_address: string;
  protocol_magic: string;
  block_timestamp: number;
  assets: [
    {
      request_id: string;
      mainchain_tx_id: string;
      asset_id: string;
      asset_value: string;
      executed_state?: boolean;
      executed_block_number?: string;
      executed_timestamp?: number;
      transaction_id: string;
    }
  ];
};

export type BridgeStatistics = {
  total_wraps: number;
  total_unwraps: number;
  total_mina_locked: number;
};

// /requests
export type BridgeTransferResponse = {
  address: string;
  amount: string;
  direction: RequestType;
  actionState: string;
};

export type BridgeTransfersResponse = {
  transfers: BridgeTransferResponse[];
};

export type BridgeRequestFollowingParams = {
  next_wrap_id: number;
  next_unwrap_id: number;
};

/** Endpoints start **/

export const getCompletedTransfers = async ({
  baseUrl,
  wrap_id,
  unwrap_id,
  limit,
  sort = "Desc",
  txStatus,
}: {
  baseUrl: string;
  wrap_id?: number | null;
  unwrap_id?: number | null;
  limit?: number | null;
  sort?: "Asc" | "Desc";
  txStatus?: TxStatus;
}): Promise<BridgeTransfersResponse> => {
  // const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
  //   sort,
  //   wrap_id: wrap_id,
  //   unwrap_id: unwrap_id,
  //   count: limit,
  //   request_status: txStatus === TxStatus.All ? null : txStatus,
  // });
  // const request = await fetch(url);
  // return await request.json();
  return new Promise((resolve) =>
    setTimeout(
      () =>
        resolve({
          transfers: [
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "123",
              direction: RequestType.WRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "123",
              direction: RequestType.UNWRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "456",
              direction: RequestType.WRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "789",
              direction: RequestType.UNWRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
          ],
        }),
      500
    )
  );
};

export const getPendingTransfers = async ({
  baseUrl,
  wrap_id,
  unwrap_id,
  limit,
  sort = "Desc",
  txStatus,
}: {
  baseUrl: string;
  wrap_id?: number | null;
  unwrap_id?: number | null;
  limit?: number | null;
  sort?: "Asc" | "Desc";
  txStatus?: TxStatus;
}): Promise<BridgeTransfersResponse> => {
  // const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
  //   sort,
  //   wrap_id: wrap_id,
  //   unwrap_id: unwrap_id,
  //   count: limit,
  //   request_status: txStatus === TxStatus.All ? null : txStatus,
  // });
  // const request = await fetch(url);
  // return await request.json();
  return new Promise((resolve) =>
    setTimeout(
      () =>
        resolve({
          transfers: [
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "123",
              direction: RequestType.WRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "123",
              direction: RequestType.UNWRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "456",
              direction: RequestType.WRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
            {
              address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
              amount: "789",
              direction: RequestType.UNWRAP,
              actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
            },
          ],
        }),
      500
    )
  );
};

export const getStatisticsAsync = async (baseUrl: string): Promise<BridgeStatistics> => {
  // const request = await fetch(`${baseUrl}/statistics`);
  // return await request.json();
  return new Promise((resolve) =>
    setTimeout(
      () =>
        resolve({
          total_mina_locked: 2857265127000000,
          total_unwraps: 39201,
          total_wraps: 86090,
        }),
      300
    )
  );
};
export const getTransferIdDetails = async ({
  baseUrl,
  id,
}: {
  baseUrl: string;
  id?: string;
}): Promise<BridgeTransferResponse> => {
  // const url = generateUrlWithQueryParams(`${baseUrl}/wrapping_requests`, {
  //   mainchain_tx_id: id,
  // });
  // const request = await fetch(url);
  // return await request.json();
  return {
    address: "B62qo8FPFHTQ2J5eaCK74yfsMRBFT5bnr2iPomWLv61iGpaufBop6Si",
    amount: "123",
    direction: RequestType.WRAP,
    actionState: "CkpYypibAWicbEVyJtYWmEgfLS1GThuzALVU3GwhNRoZKkUjDLQgE",
  };
};

export const getRequestsByMainchainAddressAsync = async ({
  baseUrl,
  address,
  limit = 10,
  txStatus,
}: {
  baseUrl: string;
  address: string;
  limit: number;
  txStatus?: TxStatus;
}): Promise<BridgeTransfersResponse> => {
  const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
    to: address,
    count: limit,
    request_status: txStatus === TxStatus.All ? null : txStatus,
  });
  const request = await fetch(url);
  return await request.json();
};

export const getRequestsByMainchainTxIdAsync = async ({
  baseUrl,
  mainchainTxId,
  limit = 10,
  txStatus,
}: {
  baseUrl: string;
  mainchainTxId: string;
  limit: number;
  txStatus?: TxStatus;
}): Promise<BridgeTransfersResponse> => {
  const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
    mainchain_tx_id: mainchainTxId,
    count: limit,
    request_status: txStatus === TxStatus.All ? null : txStatus,
  });
  const request = await fetch(url);
  return await request.json();
};

export const getRequestsByMilkomedaAddressAsync = async ({
  baseUrl,
  limit = 10,
  evmAddress,
}: {
  baseUrl: string;
  limit: number;
  evmAddress?: string;
}): Promise<BridgeTransfersResponse> => {
  const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
    count: limit,
    evm_address: evmAddress,
  });
  const request = await fetch(url);
  return await request.json();
};

export const getRequestsByMilkomedaTxIdAsync = async ({
  baseUrl,
  milkomedaTxId,
  limit = 10,
  txStatus,
}: {
  baseUrl: string;
  milkomedaTxId: string;
  limit: number;
  txStatus?: TxStatus;
}): Promise<BridgeTransfersResponse> => {
  const url = generateUrlWithQueryParams(`${baseUrl}/requests`, {
    tx_id: milkomedaTxId,
    count: limit,
    request_status: txStatus === TxStatus.All ? null : txStatus,
  });
  const request = await fetch(url);
  return await request.json();
};
/** Endpoints end **/
