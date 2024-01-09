import React, { useState } from "react";

import FiltersControls from "../../Components/UI/Molecules/FiltersControls/FiltersControls";
import TransactionCard from "../../Components/UI/Molecules/TransactionCard/TransactionCard";
import { useNavigate, useParams, useSearchParams } from "react-router-dom";
import { useInfiniteQuery, useQuery } from "react-query";
import { FilterStatus, SearchInputType, TxStatus } from "../../Types";
import {
  BridgeTransferResponse,
  getCompletedTransfers,
  getRequestsByMainchainAddressAsync,
  getRequestsByMainchainTxIdAsync,
  getRequestsByMilkomedaAddressAsync,
  getRequestsByMilkomedaTxIdAsync,
} from "../../Services/BridgeService";
import { MINA_BLOCK_TIME_MILLISECONDS, SEARCH_LIMIT } from "../../Constants";
import { RequestType, UtilService } from "../../Services/UtilService";
import { generateQueryParams } from "../../utils";
import Loader from "../../Components/UI/Atoms/Loader";
import { useInView } from "react-intersection-observer";
import { ReactComponent as TopArrowIcon } from "../../Assets/arrow-top.svg";
import { useAppProvider } from "../../context/AppContext";
import { QueryKeys } from "../../Services/ConfigService";

const MAX_RECENT_RESULTS = 50;

const goToTop = () => {
  window.scrollTo({
    top: 0,
    behavior: "smooth",
  });
};

const Search = () => {
  const navigate = useNavigate();
  const { selectedNetwork } = useAppProvider();
  const { type } = useParams<{ type: SearchInputType; network: string }>();
  let [searchParams] = useSearchParams();

  const query = searchParams.get("query") ?? "";
  const txStatusQuery = (searchParams.get("txStatus") ?? TxStatus.All) as TxStatus;
  const filterStatusQuery = (searchParams.get("filterStatus") ?? TxStatus.All) as FilterStatus;

  const [value, setValue] = useState<string>(query);
  const [txStatus, setTxStatus] = useState<TxStatus | FilterStatus>(txStatusQuery);

  const { ref, inView } = useInView();
  const [showBottomButton, setShowBottomButton] = useState(false);

  React.useEffect(() => {
    window.addEventListener("scroll", () => {
      if (window.scrollY > 400) {
        setShowBottomButton(true);
      } else {
        setShowBottomButton(false);
      }
    });
  }, []);

  const {
    isLoading: isSearchResultsLoading,
    isIdle: isSearchResultsIdle,
    data: searchResults,
  } = useQuery(
    [QueryKeys.SEARCH, type, query, txStatusQuery, filterStatusQuery],
    async () => {
      let requests: BridgeTransferResponse[] = [];
      switch (type) {
        case SearchInputType.MainchainAddress:
          requests = (
            await getRequestsByMainchainAddressAsync({
              baseUrl: selectedNetwork.bridgeApiBaseURL,
              address: query.toString(),
              limit: SEARCH_LIMIT,
              txStatus: txStatusQuery,
            })
          ).transfers;
          break;
        case SearchInputType.MainchainTxId:
          requests = (
            await getRequestsByMainchainTxIdAsync({
              baseUrl: selectedNetwork.bridgeApiBaseURL,
              mainchainTxId: query.toString(),
              limit: SEARCH_LIMIT,
              txStatus: txStatusQuery,
            })
          ).transfers;
          break;
        case SearchInputType.EvmAddress:
          requests = (
            await getRequestsByMilkomedaAddressAsync({
              baseUrl: selectedNetwork.bridgeApiBaseURL,
              limit: SEARCH_LIMIT,
              evmAddress: query,
            })
          ).transfers;
          break;
        case SearchInputType.MilkomedaTxId:
          requests = (
            await getRequestsByMilkomedaTxIdAsync({
              baseUrl: selectedNetwork.bridgeApiBaseURL,
              milkomedaTxId: query.toString(),
              limit: SEARCH_LIMIT,
              txStatus: txStatusQuery,
            })
          ).transfers;
          break;
      }
      if (requests.length > 0) {
        return requests;
      }
      return [];
    },
    {
      enabled: query.length > 0 && (!!type || !!txStatusQuery),
    }
  );

  const recentRequestQuery = useInfiniteQuery(
    ["requests", txStatusQuery],
    async ({
      pageParam = {
        wrapId: null,
        unwrapId: null,
      },
    }) =>
      await getCompletedTransfers({
        baseUrl: selectedNetwork.bridgeApiBaseURL,
        wrap_id: pageParam.wrapId,
        unwrap_id: pageParam.unwrapId,
        txStatus: txStatusQuery,
        limit: pageParam.wrapId == null && pageParam.unwrapId == null ? MAX_RECENT_RESULTS : 10,
      }),
    {
      getNextPageParam: (lastPage) => {
        if (lastPage?.transfers.length === 0) return;
        return {
          // wrapId: lastPage.following_params.next_wrap_id,
          // unwrapId: lastPage.following_params.next_unwrap_id,
        };
      },
      refetchInterval: MINA_BLOCK_TIME_MILLISECONDS,
      enabled: !!txStatusQuery,
    }
  );

  React.useEffect(() => {
    if (inView) {
      recentRequestQuery.fetchNextPage();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [inView]);

  const onSearchClicked = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    const valueTrimmed = value.trim();
    const queryParams = generateQueryParams({
      query: valueTrimmed,
      txStatus: txStatus === TxStatus.All ? null : txStatus,
    });

    if (txStatus === FilterStatus.EvmAddress) {
      if (valueTrimmed === "") {
        return;
      }

      const queryParams = generateQueryParams({
        query: valueTrimmed,
        filterStatus: txStatus === FilterStatus.EvmAddress ? null : txStatus,
      });

      navigate({
        pathname: `/${selectedNetwork.id}/search/${SearchInputType.EvmAddress}`,
        search: queryParams,
      });
      return;
    }

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

  return (
    <div className="position mx-auto flex w-full max-w-7xl flex-wrap px-5 py-9 pt-24">
      <FiltersControls
        onSearchClicked={onSearchClicked}
        searchValueState={[value, setValue]}
        txStatusState={[txStatus, setTxStatus]}
      />
      <div className="w-full">
        <h2 className="mt-7 text-lg font-bold text-white">All Requests</h2>
        {recentRequestQuery.isLoading ? (
          <Loader className="mt-5 flex h-6 w-full justify-center text-white md:mt-10 md:h-9" />
        ) : isSearchResultsIdle ? (
          <>
            {recentRequestQuery.data?.pages.map((requestPage) => {
              const requests = requestPage.transfers;
              return requests.map((requestItem) => {
                let requestType = requestItem.direction;
                return (
                  <button
                    className="my-4 w-full text-left"
                    key={`${requestItem.actionState}`}
                    onClick={() =>
                      navigate(
                        requestType === RequestType.WRAP
                          ? `/${selectedNetwork.id}/wrap/${requestItem.actionState}`
                          : `/${selectedNetwork.id}/unwrap/${requestItem.actionState}`,
                        { state: { fromPathname: "/search" } }
                      )
                    }
                  >
                    <TransactionCard requestItem={requestItem} requestType={requestType} />
                  </button>
                );
              });
            })}
            <button
              ref={ref}
              className="primary-btn mx-auto my-5 block h-fit w-full md:my-0 md:w-[120px]"
              disabled={!recentRequestQuery.hasNextPage || recentRequestQuery.isFetchingNextPage}
              onClick={() => recentRequestQuery.fetchNextPage()}
            >
              {recentRequestQuery.isFetchingNextPage ? (
                <Loader className="flex w-full justify-center text-white" />
              ) : recentRequestQuery.hasNextPage ? (
                "Load more"
              ) : (
                "All requests has been loaded."
              )}
            </button>
          </>
        ) : (
          <>
            {isSearchResultsLoading ? (
              <Loader className="mt-5 flex h-6 w-full justify-center text-white md:mt-10 md:h-9" />
            ) : searchResults != null && searchResults?.length > 0 ? (
              searchResults.map((requestItem: BridgeTransferResponse) => {
                let requestType = requestItem.direction;
                return (
                  <button
                    className="my-4 text-left"
                    key={`${requestItem.actionState}`}
                    onClick={() =>
                      navigate(
                        requestType === RequestType.WRAP
                          ? `/${selectedNetwork.id}/wrap/${requestItem.actionState}`
                          : `/${selectedNetwork.id}/unwrap/${requestItem.actionState}`,
                        { state: { fromPathname: "/search" } }
                      )
                    }
                  >
                    <TransactionCard requestItem={requestItem} requestType={requestType} />
                  </button>
                );
              })
            ) : searchResults && !searchResults.length ? (
              <div className="mx-auto mt-10 text-center">
                <h2 className="mt-2 text-xl font-bold text-white">No results found</h2>
                <p className="mt-2 text-sm text-white">
                  Try adjusting your search or filter to find what you're looking for.
                </p>
              </div>
            ) : null}
          </>
        )}
        {showBottomButton && (
          <button
            className="fixed bottom-3 right-3 flex h-10 w-10 animate-fadeIn items-center justify-center rounded-full bg-orange-600 text-white transition-all hover:bg-orange-700"
            onClick={goToTop}
          >
            <span className="sr-only">Scroll to Top</span>
            <TopArrowIcon />
          </button>
        )}
      </div>
    </div>
  );
};

export default Search;
