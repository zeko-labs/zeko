import React from "react";
import Arrow from "../../../../Assets/arrow.svg";
import ZekoIcon from "../../../../Assets/logos/zeko-icon.png";
import MinaIcon from "../../../../Assets/logos/mina-icon.png";
import { ProcessedBridgeRequestResponse } from "../../../../Types";
import { UtilService } from "../../../../Services/UtilService";
import { useAppProvider } from "../../../../context/AppContext";

type TransactionDataProps = {
  requestItem: ProcessedBridgeRequestResponse;
};

const ExternalLink = ({ children, href }: { children: React.ReactNode; href: string }) => (
  <a
    className="transition duration-300 ease-out hover:text-sky-400"
    href={href}
    target="_blank"
    rel="noopener noreferrer"
    onClick={(e) => e.stopPropagation()}
  >
    {children}
  </a>
);

function TransactionData({ requestItem }: TransactionDataProps) {
  const { selectedNetwork } = useAppProvider();
  return (
    <div>
      <p className="my-2 text-sm font-semibold text-white md:my-0">
        <span className="text-gray-200">
          MINA ({requestItem.direction === "WRAP" ? "Wrapped" : "Unwrapped"}
          ):{" "}
        </span>
        <span>{requestItem.amount} </span>
      </p>
      <div className="mt-2 flex flex-col items-start break-all md:mt-4 md:flex-row md:items-center">
        <div className="flex md:flex-1 md:basis-full">
          <img
            className="mr-4 h-full w-8 min-w-[2rem] self-center"
            src={requestItem.direction === "WRAP" ? MinaIcon : ZekoIcon}
            alt={requestItem.direction === "WRAP" ? "Mina" : "Zeko"}
          />
          <div>
            <p className="my-2 text-sm font-semibold text-white md:my-0">
              <span className="text-gray-200">From</span>
            </p>
            <p className="my-2 text-sm font-semibold text-white md:my-0">
              <span className="break-all underline">
                <ExternalLink
                  href={`${UtilService.getExplorerFromTxUrlBase(
                    selectedNetwork,
                    requestItem.direction
                  )}/${requestItem.actionState}`}
                >
                  {requestItem.address}
                </ExternalLink>
              </span>
            </p>
          </div>
        </div>
        <div className="hidden md:block md:min-w-[60px] md:pb-0">
          <img className="mx-auto" src={Arrow} alt="" />
        </div>
        <div className="flex md:flex-1 md:basis-full">
          <img
            className="mr-4 h-full w-8 min-w-[2rem] self-center"
            src={requestItem.direction === "WRAP" ? ZekoIcon : MinaIcon}
            alt={requestItem.direction === "WRAP" ? "Zeko" : "Mina"}
          />
          <div>
            <p className="my-2 text-sm font-semibold text-gray-200 md:my-0">
              <span>To</span>
            </p>
            <p className="my-2 text-sm font-semibold text-white md:my-0">
              <span className="break-all underline">
                <ExternalLink
                  href={`${UtilService.getExplorerToTxUrlBase(
                    selectedNetwork,
                    requestItem.direction
                  )}/${requestItem.actionState}`}
                >
                  {requestItem.address}
                </ExternalLink>
              </span>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}

export default TransactionData;
