import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useQuery } from "react-query";
import { SearchInputType } from "../../Types";
import {
  getCompletedTransfers,
  getStatisticsAsync,
  BridgeTransferResponse,
  getPendingTransfers,
} from "../../Services/BridgeService";
import { MINA_BLOCK_TIME_MILLISECONDS } from "../../Constants";
import { RequestType, UtilService } from "../../Services/UtilService";
import BoxStats from "../../Components/UI/Atoms/BoxStats/BoxStats";
import TitleContainer from "../../Components/UI/Atoms/TitleContainer/TitleContainer";
import TransactionCard from "../../Components/UI/Molecules/TransactionCard/TransactionCard";
import IconWrapUnwrap from "../../Assets/icon_wrap_unwrap.svg";
import IconWrap from "../../Assets/icon_wrap.svg";
import IconUnwrap from "../../Assets/icon_unwrap.svg";
import BigNumber from "bignumber.js";
import { generateQueryParams } from "../../utils";
import Loader from "../../Components/UI/Atoms/Loader";
import PullToRefresh from "react-simple-pull-to-refresh";
import { useAppProvider } from "../../context/AppContext";
import { QueryKeys } from "../../Services/ConfigService";

const MAX_RECENT_RESULTS = 10;

function Home() {
  const { selectedNetwork } = useAppProvider();

  const navigate = useNavigate();
  const [value, setValue] = useState<string>("");
  // TODO: Refactor, it might unnecessary to create a new state based on a react query request
  const [renderedCompletedTransfers, setRenderedCompletedTransfers] = useState<
    BridgeTransferResponse[] | undefined
  >();
  const [renderedPendingTransfers, setRenderedPendingTransfers] = useState<
    BridgeTransferResponse[] | undefined
  >();

  const {
    isLoading: isBridgeStatsLoading,
    isSuccess: isBridgeStatsSuccess,
    data: bridgeStats,
    refetch: refetchBridgeStats,
  } = useQuery(
    [QueryKeys.BRIDGE_STATS],
    async () => await getStatisticsAsync(selectedNetwork.bridgeApiBaseURL),
    {
      refetchInterval: MINA_BLOCK_TIME_MILLISECONDS,
    }
  );

  const {
    isLoading: isRecentCompletedTransfersLoading,
    isSuccess: isRecentCompletedTransfersSuccess,
    data: recentCompletedTransfers,
    refetch: refetchRecentCompletedTransfers,
  } = useQuery(
    [QueryKeys.COMPLETED_TRANSFERS],
    async () => {
      const response = await getCompletedTransfers({
        baseUrl: selectedNetwork.bridgeApiBaseURL,
        limit: MAX_RECENT_RESULTS,
      });

      if (!renderedCompletedTransfers || renderedCompletedTransfers?.length === 0) {
        return response.transfers;
      }
      // if the first request from both arrays is different, then there's new request to show
      if (
        renderedCompletedTransfers &&
        response.transfers[0].actionState !== renderedCompletedTransfers[0].actionState
      ) {
        return response.transfers;
      }
      return [];
    },
    {
      refetchInterval: MINA_BLOCK_TIME_MILLISECONDS,
    }
  );

  const {
    isLoading: isRecentPendingTransfersLoading,
    isSuccess: isRecentPendingTransfersSuccess,
    data: recentPendingTransfers,
    refetch: refetchRecentPendingTransfers,
  } = useQuery(
    [QueryKeys.PENDING_TRANSFERS],
    async () => {
      const response = await getPendingTransfers({
        baseUrl: selectedNetwork.bridgeApiBaseURL,
        limit: MAX_RECENT_RESULTS,
      });

      if (!renderedPendingTransfers || renderedPendingTransfers?.length === 0) {
        return response.transfers;
      }
      // if the first request from both arrays is different, then there's new request to show
      if (
        renderedPendingTransfers &&
        response.transfers[0].actionState !== renderedPendingTransfers[0].actionState
      ) {
        return response.transfers;
      }
      return [];
    },
    {
      refetchInterval: MINA_BLOCK_TIME_MILLISECONDS,
    }
  );

  const onSearchClicked = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    const valueTrimmed = value.trim();
    if (!valueTrimmed) return;
    const queryParams = generateQueryParams({
      query: valueTrimmed,
    });
    // Check if search value is EVM Transaction ID
    if (UtilService.isEvmTxIdAsync(valueTrimmed)) {
      navigate({
        pathname: `/${selectedNetwork.id}/search/${SearchInputType.MilkomedaTxId}`,
        search: queryParams,
      });
    } else {
      // if not evm, then it has to be mainchain or it's incorrect
      navigate({
        pathname: `/${selectedNetwork.id}/search/${SearchInputType.MainchainTxId}`,
        search: queryParams,
      });
    }
  };

  // process the results and render them
  useEffect(() => {
    if (renderedCompletedTransfers == null) {
      setRenderedCompletedTransfers(recentCompletedTransfers ?? []);
    } else if (recentCompletedTransfers && recentCompletedTransfers?.length > 0) {
      let newRequestItems = recentCompletedTransfers.filter(
        (i) => renderedCompletedTransfers.find((j) => j.actionState === i.actionState) == null
      );

      const newRenderedRequests = [...newRequestItems, ...renderedCompletedTransfers];
      setRenderedCompletedTransfers(newRenderedRequests.slice(0, MAX_RECENT_RESULTS));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [recentCompletedTransfers]);
  useEffect(() => {
    if (renderedPendingTransfers == null) {
      setRenderedPendingTransfers(recentPendingTransfers ?? []);
    } else if (recentPendingTransfers && recentPendingTransfers?.length > 0) {
      let newRequestItems = recentPendingTransfers.filter(
        (i) => renderedPendingTransfers.find((j) => j.actionState === i.actionState) == null
      );

      const newRenderedRequests = [...newRequestItems, ...renderedPendingTransfers];
      setRenderedPendingTransfers(newRenderedRequests.slice(0, MAX_RECENT_RESULTS));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [recentPendingTransfers]);

  const getTotalBridgeTxCount = () =>
    isBridgeStatsSuccess ? bridgeStats.total_wraps + bridgeStats.total_unwraps : "-";

  // todo: cleanup
  let totalMinaLocked = isBridgeStatsSuccess
    ? new BigNumber(bridgeStats.total_mina_locked)
        .div(new BigNumber("1000000000"))
        .toNumber()
        .toLocaleString()
    : "-";

  return (
    <PullToRefresh
      onRefresh={async () => {
        await refetchBridgeStats();
        await refetchRecentCompletedTransfers();
        await refetchRecentPendingTransfers();
      }}
      refreshingContent={
        <div className="flex w-full justify-center pt-28 ">
          <Loader className="h-5 text-white" />
        </div>
      }
    >
      <div className="mx-auto flex w-full max-w-7xl flex-wrap px-5 py-9 pt-24">
        <div className="grid w-full grid-cols-1 gap-4 sm:grid-cols-2 xl:grid-cols-4">
          <BoxStats
            isLoading={isBridgeStatsLoading}
            text="Total Bridge Txs"
            value={getTotalBridgeTxCount().toLocaleString()}
            img={<img src={IconWrapUnwrap} alt="bridge-icon" />}
          />
          <BoxStats
            isLoading={isBridgeStatsLoading}
            text={`Total MINA Locked`}
            value={totalMinaLocked}
          />
          <BoxStats
            isLoading={isBridgeStatsLoading}
            text="Total Wraps"
            value={isBridgeStatsSuccess ? bridgeStats?.total_wraps.toLocaleString() : "-"}
            img={<img src={IconWrap} alt="wrap-icon" />}
          />
          <BoxStats
            isLoading={isBridgeStatsLoading}
            text="Total Unwraps"
            value={isBridgeStatsSuccess ? bridgeStats?.total_unwraps.toLocaleString() : "-"}
            img={<img src={IconUnwrap} alt="unwrap-icon" />}
          />
        </div>
        <div className="mx-1 my-6 flex w-full flex-wrap content-between">
          <TitleContainer
            text="Last 10 Completed Transfers"
            onSearchClicked={onSearchClicked}
            value={value}
            setValue={setValue}
          />
          {isRecentCompletedTransfersLoading ? (
            <div className="mt-5 flex flex-1 items-center justify-center md:mt-10 md:h-9">
              <Loader className="text-orange-600 md:h-9" />
            </div>
          ) : null}
          {isRecentCompletedTransfersSuccess &&
            renderedCompletedTransfers != null &&
            renderedCompletedTransfers.map((requestItem) => {
              let requestType = RequestType.WRAP;
              if (requestItem?.direction === RequestType.UNWRAP) {
                requestType = RequestType.UNWRAP;
              }
              return (
                <button
                  className="my-4 w-full text-left"
                  key={`${requestItem.actionState}`}
                  onClick={() =>
                    navigate(`/${selectedNetwork.id}/tx/${requestItem.actionState}`, {
                      state: { fromPathname: "/" },
                    })
                  }
                >
                  <TransactionCard requestItem={requestItem} requestType={requestType} />
                </button>
              );
            })}
        </div>
        <div className="mx-1 my-6 flex w-full flex-wrap content-between">
          <TitleContainer
            text="Last 10 Pending Transfers"
            onSearchClicked={onSearchClicked}
            value={value}
            setValue={setValue}
          />
          {isRecentPendingTransfersLoading ? (
            <div className="mt-5 flex flex-1 items-center justify-center md:mt-10 md:h-9">
              <Loader className="text-orange-600 md:h-9" />
            </div>
          ) : null}
          {isRecentPendingTransfersSuccess &&
            renderedPendingTransfers != null &&
            renderedPendingTransfers.map((requestItem) => {
              let requestType = RequestType.WRAP;
              if (requestItem?.direction === RequestType.UNWRAP) {
                requestType = RequestType.UNWRAP;
              }
              return (
                <button
                  className="my-4 w-full text-left"
                  key={`${requestItem.actionState}`}
                  onClick={() =>
                    navigate(`/${selectedNetwork.id}/tx/${requestItem.actionState}`, {
                      state: { fromPathname: "/" },
                    })
                  }
                >
                  <TransactionCard requestItem={requestItem} requestType={requestType} />
                </button>
              );
            })}
        </div>
      </div>
    </PullToRefresh>
  );
}

export default Home;
