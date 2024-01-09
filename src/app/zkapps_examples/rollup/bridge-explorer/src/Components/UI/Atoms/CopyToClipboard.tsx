import React from "react";
import { CopyToClipboard as CopyClipboardBase } from "react-copy-to-clipboard";
import { ReactComponent as CopyIcon } from "../../../Assets/copy-clipboard.svg";
import * as Tooltip from "@radix-ui/react-tooltip";

const CopyToClipboard = ({ text, children }: { text: string; children: React.ReactNode }) => {
  const [copySuccess, setCopySuccess] = React.useState(false);
  const [tooltipOpen, setTooltipOpen] = React.useState(false);

  React.useEffect(() => {
    setTimeout(() => {
      setCopySuccess(false);
      setTooltipOpen(false);
    }, 2000);
  }, [copySuccess]);

  return (
    <Tooltip.Provider delayDuration={0}>
      <Tooltip.Root open={tooltipOpen} onOpenChange={() => {}}>
        <Tooltip.Trigger
          onMouseEnter={() => setTooltipOpen(true)}
          onMouseLeave={() => setTooltipOpen(false)}
          className="rounded-md p-1 text-left hover:bg-gray-800/80"
        >
          <CopyClipboardBase text={text} onCopy={() => setCopySuccess(true)}>
            <span>
              <span className="break-all">{children}</span>
              <span className="ml-2 inline-flex h-6 w-6 items-center justify-center rounded-md align-sub text-white">
                <CopyIcon className="h-5 w-5" />
              </span>
            </span>
          </CopyClipboardBase>
        </Tooltip.Trigger>
        <Tooltip.Content
          sideOffset={4}
          className="animate-fadeIn rounded-md bg-slate-900 py-2 px-3 text-center text-sm text-white shadow-lg duration-75 ease-out"
        >
          <Tooltip.Arrow className="fill-slate-900" />
          {copySuccess ? "Copied!" : "Copy to clipboard"}
        </Tooltip.Content>
      </Tooltip.Root>
    </Tooltip.Provider>
  );
};

export default CopyToClipboard;
