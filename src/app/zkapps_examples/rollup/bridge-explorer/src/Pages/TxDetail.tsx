import { useQuery } from "react-query";
import { useLocation, useNavigate, useParams } from "react-router-dom";
import Loader from "../Components/UI/Atoms/Loader";

import { ReactComponent as ChevronLeft } from "../Assets/chevron-left.svg";
import NotFound from "../Components/UI/Molecules/NotFound";
import { useAppProvider } from "../context/AppContext";
import { RequestType, UtilService } from "../Services/UtilService";
import { LocationState } from "../Types";
import TransactionCard from "../Components/UI/Molecules/TransactionCard/TransactionCard";
import * as Tabs from "@radix-ui/react-tabs";
import { getTransferIdDetails } from "../Services/BridgeService";
import { QueryKeys } from "../Services/ConfigService";

const LabelValueRow = ({ label, value, href }: { label: string; value: string; href?: string }) => {
  const valueComponent =
    href != null ? (
      <dl className="break-all text-white ">
        <a
          className="break-all text-white underline transition duration-300 ease-out hover:text-sky-400"
          href={href}
          target="_blank"
          rel="noreferrer noopener"
        >
          {value}
        </a>
      </dl>
    ) : (
      <dl className="break-all text-white">{value}</dl>
    );

  return (
    <div className="my-2 flex flex-col text-sm font-semibold md:flex-row">
      <dt className="min-w-[10rem] basis-0 text-gray-200 md:basis-40">{label}:</dt>
      {valueComponent}
    </div>
  );
};

export default function TxDetail() {
  const { id } = useParams();
  const { selectedNetwork } = useAppProvider();
  const navigate = useNavigate();
  const location = useLocation();
  const { fromPathname } = (location.state as LocationState) ?? { fromPathname: null };
  const txDetail = useQuery(
    [QueryKeys.TRANSFER_DETAIL, id],
    () => {
      return getTransferIdDetails({
        baseUrl: selectedNetwork.bridgeApiBaseURL,
        id,
      });
    },
    { enabled: !!id }
  );
  return (
    <div className="position mx-auto flex w-full max-w-7xl flex-col px-5 py-9 pt-24">
      <button
        onClick={() => {
          if (fromPathname) {
            navigate(`/${selectedNetwork.id}${fromPathname}`);
            return;
          }
          navigate(`/${selectedNetwork.id}`);
        }}
        className="flex items-center"
      >
        <ChevronLeft className="mr-1" />
        <p className="text-sm text-gray-200">Back</p>
      </button>
      <h1 className="mt-3 mb-4 text-lg font-bold text-white">Transaction Details</h1>
      {txDetail.isLoading ? (
        <Loader className="mt-5 flex h-6 w-full justify-center text-white md:mt-10 md:h-9" />
      ) : txDetail.isSuccess && txDetail.data == null ? (
        <NotFound />
      ) : (
        txDetail.data && (
          <>
            <TransactionCard requestType={txDetail.data.direction} requestItem={txDetail.data} />
            <div className="mt-5 animate-fadeIn text-white transition-all duration-100">
              <Tabs.Root defaultValue="details" className="detail-tab">
                <Tabs.List className="tab-list">
                  <Tabs.Trigger value="details" className="tab">
                    {txDetail.data.direction === RequestType.WRAP ? "Wrapping" : "Unwrapping"}{" "}
                    Request
                  </Tabs.Trigger>
                </Tabs.List>
                <Tabs.Content value="details" className="tab-content">
                  <LabelValueRow label="Status" value={"tbd"} />
                  <LabelValueRow label="Address" value={txDetail.data.address} />
                  <LabelValueRow label="Amount" value={txDetail.data.amount} />
                  <LabelValueRow
                    href={`${UtilService.getExplorerFromTxUrlBase(
                      selectedNetwork,
                      txDetail.data.direction
                    )}/${txDetail.data.actionState}`}
                    label={`From Tx`}
                    value={txDetail.data.actionState}
                  />
                  <LabelValueRow
                    href={`${UtilService.getExplorerToTxUrlBase(
                      selectedNetwork,
                      txDetail.data.direction
                    )}/${txDetail.data.actionState}`}
                    label={`To Tx`}
                    value={txDetail.data.actionState}
                  />
                </Tabs.Content>
              </Tabs.Root>
            </div>
          </>
        )
      )}
    </div>
  );
}
