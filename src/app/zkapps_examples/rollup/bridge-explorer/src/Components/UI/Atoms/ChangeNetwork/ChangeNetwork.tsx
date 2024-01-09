import React, { Fragment } from "react";
import { Listbox, Transition } from "@headlessui/react";
import { ReactComponent as DropdownArrowIcon } from "../../../../Assets/dropdown-arrow-down.svg";
import { ReactComponent as CheckIcon } from "../../../../Assets/check-icon.svg";

import { networkList } from "../../../../App";
import { useAppProvider } from "../../../../context/AppContext";
import { useLocation, useNavigate } from "react-router-dom";
import classNames from "clsx";
import MinaIcon from "../../../../Assets/logos/mina-icon.png";

const ChangeNetwork = ({
  onNetworkDropdownClose,
  className,
}: {
  onNetworkDropdownClose?: (value: void) => void;
  className?: string;
}) => {
  const { selectedNetwork, setSelectedNetwork } = useAppProvider();
  const navigate = useNavigate();
  const { pathname } = useLocation();

  return (
    <div className={classNames("flex items-center", className)}>
      <Listbox
        value={selectedNetwork}
        onChange={(value) => {
          setSelectedNetwork(value);
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          const [_, __, ...restPath] = pathname.split("/");
          // if pathname is coming from wrap/unwrap page, we need to redirect homepage
          if (restPath.includes("wrap") || restPath.includes("unwrap") || restPath.length === 0) {
            navigate(`/${value.id}`);
          } else {
            navigate(`/${value.id}/${restPath[0]}`);
          }
          onNetworkDropdownClose?.();
        }}
      >
        {({ open }) => (
          <>
            <div className="relative">
              <Listbox.Label className="sr-only">Choose Network:</Listbox.Label>
              <Listbox.Button
                className={classNames(
                  "shadow-[0px 0px 120px rgba(255, 255, 255, 0.12)] relative flex h-[40px] min-w-[192px] cursor-pointer items-center rounded-md bg-black/25 px-1 pr-7 text-left font-bold capitalize text-orange-600 sm:text-sm",
                  "transition focus:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-orange-600"
                  // open && "bg-[#DDDBDA]"
                )}
              >
                <div className="flex items-center">
                  <img className="mr-2 h-7 w-7" src={MinaIcon} alt="" />
                  <span className="block text-white">{selectedNetwork.name}</span>
                </div>
                <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2 text-white">
                  <DropdownArrowIcon className={open ? "rotate-180 transform" : ""} />
                </span>
              </Listbox.Button>
              <Transition
                as={Fragment}
                leave="transition ease-in duration-100"
                leaveFrom="opacity-100"
                leaveTo="opacity-0"
              >
                <Listbox.Options className="absolute mt-1 max-h-60 w-full overflow-auto rounded bg-[#222222] py-1 text-sm text-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
                  {networkList.map((network, idx) => (
                    <Listbox.Option
                      key={network.id}
                      className={({ selected, disabled }) =>
                        classNames(
                          "relative flex h-[40px] cursor-pointer select-none px-[6px]",
                          idx % 2 === 1 &&
                            networkList.length - 1 !== idx &&
                            "mb-1 border-b-[1px] border-b-[#353535]",
                          selected
                            ? "bg-gradient-secondary text-white hover:text-white"
                            : "hover:bg-gradient-secondary text-[#919191] hover:text-white",
                          disabled &&
                            "hover:bg-gradient-none cursor-not-allowed text-[#919191] opacity-60 hover:text-[#919191]"
                        )
                      }
                      // disabled={network.id === Networks.algorandMainnet}
                      value={network}
                    >
                      {({ selected }) => (
                        <>
                          <div className="flex items-center">
                            {network?.hideIcon ? (
                              <div
                                className="mr-2 flex h-7 w-7 items-center justify-center"
                                aria-hidden="true"
                              >
                                <div
                                  className={classNames(
                                    "relative z-10 h-[5px] w-[5px] rounded-full",
                                    "before:absolute before:-top-[24px] before:left-[2px] before:z-[1] before:h-[24px] before:w-[1px] before:bg-[#444]",
                                    selected ? "bg-white" : "bg-[#444]"
                                  )}
                                />
                              </div>
                            ) : (
                              <img className="mr-2 h-7 w-7" src={MinaIcon} alt="" />
                            )}
                            <span className="block text-sm font-semibold capitalize">
                              {network.name}
                            </span>
                          </div>
                          {selected ? (
                            <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2 ">
                              <CheckIcon />
                            </span>
                          ) : null}
                        </>
                      )}
                    </Listbox.Option>
                  ))}
                </Listbox.Options>
              </Transition>
            </div>
          </>
        )}
      </Listbox>
    </div>
  );
};

export default ChangeNetwork;
