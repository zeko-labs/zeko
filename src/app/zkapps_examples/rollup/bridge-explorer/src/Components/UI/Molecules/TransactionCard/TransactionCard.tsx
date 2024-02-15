import clsx from "clsx";
import { AssetDataMapItem, RequestType } from "../../../../Services/UtilService";
import { ProcessedBridgeRequestResponse, TxStatus } from "../../../../Types";

import TransactionData from "./TransactionData";

type TransactionCardProps = {
  requestType: RequestType;
  requestItem: ProcessedBridgeRequestResponse;
  wrapItem?: ProcessedBridgeRequestResponse;
  unwrapItem?: ProcessedBridgeRequestResponse;
  assetDataMap?: Record<string, AssetDataMapItem>;
};

function TransactionCard({ requestItem, requestType }: TransactionCardProps) {
  const executionState = TxStatus.Confirmed;

  return (
    <div className="flex w-full max-w-6xl animate-fadeIn flex-wrap rounded-[10px] bg-gray-600 shadow-default transition-all duration-100 md:rounded-md">
      <div className="bg-gradient-primary flex w-full flex-col justify-center rounded-t-[10px] py-4 px-6 md:w-[200px] md:rounded-none md:rounded-l-md">
        <h2 className="text-2xl font-normal uppercase text-white">
          {requestType === RequestType.WRAP ? RequestType.WRAP : RequestType.UNWRAP}
        </h2>
        <div className="flex items-center text-sm font-bold text-white">
          <div
            className={clsx(
              "h-[6px] w-[6px] rounded-full",
              executionState === TxStatus.Confirmed ? "bg-orange-600" : "bg-orange-800"
            )}
          />
          <p
            className={clsx(
              "pl-2 text-xs font-bold",
              executionState === TxStatus.Confirmed ? "text-orange-600" : "text-orange-800"
            )}
          >
            {executionState.toString()}
          </p>
        </div>
        <p className="text-sm font-bold text-white">MINA</p>
      </div>
      <div className="w-full p-6 md:flex-1 md:pr-10">
        <TransactionData requestItem={requestItem} />
      </div>
    </div>
  );
}

export default TransactionCard;
