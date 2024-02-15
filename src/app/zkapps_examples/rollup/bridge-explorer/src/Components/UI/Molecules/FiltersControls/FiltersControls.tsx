import React, { Fragment, useRef } from "react";
import { ReactComponent as DropdownArrowIcon } from "../../../../Assets/dropdown-arrow-down.svg";
import { Listbox, Transition } from "@headlessui/react";
import { ReactComponent as CloseIcon } from "../../../../Assets/close-icon.svg";
import { FilterStatus, TxStatus } from "../../../../Types";
import { Network } from "../../../../Services/ConfigService";

function handleChange(value: string) {
  console.log(`selected ${value}`);
}

type FilterControlProps = {
  onSearchClicked: (e: React.FormEvent<HTMLFormElement>) => void;
  txStatusState: [string, React.Dispatch<TxStatus>];
  searchValueState: [string, React.Dispatch<string>];
  selectedNetwork?: Network;
};

function FilterSelect({
  label,
  options,
  value,
  onChange,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
  options: { value: string; label: string }[];
}) {
  return (
    <Listbox value={value} onChange={onChange}>
      <div className="relative mb-3 w-full md:mr-3 md:mb-0 md:w-auto">
        <Listbox.Label className="mb-2 block text-xs font-semibold text-gray-200">
          {label}
        </Listbox.Label>
        <Listbox.Button className="font-regular relative flex h-[32px] w-full cursor-pointer items-center bg-white p-2 pl-3 text-left capitalize transition duration-300 ease-out hover:border-orange-600 hover:ring-2 hover:ring-inset hover:ring-orange-600 focus:border-orange-600 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-orange-600 md:w-[170px]">
          <span className="block truncate">{value}</span>
          <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2 text-orange-600 ">
            <DropdownArrowIcon className="text-[#222]" />
          </span>
        </Listbox.Button>
        <Transition
          as={Fragment}
          leave="transition ease-in duration-100"
          leaveFrom="opacity-100"
          leaveTo="opacity-0"
        >
          <Listbox.Options className="absolute z-30 mt-1 max-h-60 w-full overflow-auto rounded-sm bg-white text-sm text-gray-800 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
            {options.map((option) => (
              <Listbox.Option
                key={option.value}
                className={({ active }) =>
                  `cursor-pointer select-none p-2 py-[5px] pl-3 ${
                    active ? "bg-gray-50 " : "bg-white"
                  }`
                }
                value={option.value}
              >
                <span className="block truncate">{option.label}</span>
              </Listbox.Option>
            ))}
          </Listbox.Options>
        </Transition>
      </div>
    </Listbox>
  );
}

export function SearchInput({
  label,
  value,
  setValue,
  placeholder,
  containerClassName = "",
  id,
  hideLabel = false,
}: {
  label?: string;
  placeholder: string;
  value: string;
  setValue: (value: string) => void;
  containerClassName?: string;
  id: string;
  hideLabel?: boolean;
}) {
  const inputRef = useRef<HTMLInputElement | null>(null);
  const containerClassNames = `w-full md:w-auto ${containerClassName}`.trim();
  return (
    <div className={containerClassNames}>
      <label
        htmlFor={id}
        className={`mb-2 block text-xs font-semibold text-gray-200 ${hideLabel ? "sr-only" : ""}`}
      >
        {label}
      </label>
      <div className="relative md:w-[270px]">
        <input
          id={id}
          type="text"
          className="input pr-6 transition duration-300 ease-out"
          placeholder={placeholder}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) => setValue(e.target.value)}
          value={value}
          ref={inputRef}
        />
        {value !== "" && (
          <button
            type="button"
            className="absolute inset-y-4 right-1 flex items-center"
            onClick={() => {
              setValue("");
              inputRef?.current?.focus();
            }}
          >
            <CloseIcon className="h-6 w-6 p-1.5 text-gray-400 transition duration-300 ease-out hover:text-gray-600" />
          </button>
        )}
      </div>
    </div>
  );
}

function FiltersControls({ onSearchClicked, txStatusState, searchValueState }: FilterControlProps) {
  const [txStatus, setTxStatus] = txStatusState;
  const [searchValue, setSearchValue] = searchValueState;

  const filterOptions: any = [
    { label: "All", value: TxStatus.All },
    { label: "Confirmed", value: TxStatus.Confirmed },
    { label: "Pending", value: TxStatus.Pending },
    { label: "Failed", value: TxStatus.Failed },
    { label: "Address", value: FilterStatus.EvmAddress },
  ];

  return (
    <form className="flex w-full flex-wrap items-end md:flex-nowrap" onSubmit={onSearchClicked}>
      <FilterSelect
        label="Sort by:"
        options={[{ label: "Newest", value: "newest" }]}
        value="Newest"
        onChange={handleChange}
      />
      <FilterSelect
        label="Filter by:"
        options={filterOptions}
        value={txStatus}
        onChange={(value) => {
          setTxStatus(value as TxStatus);
        }}
      />
      <SearchInput
        containerClassName="md:mr-3"
        id="search"
        placeholder={txStatus === FilterStatus.EvmAddress ? "0x..." : "TX ID"}
        value={searchValue}
        setValue={setSearchValue}
      />
      <button className="primary-btn my-4 w-full md:my-0 md:w-auto" type="submit">
        Search
      </button>
    </form>
  );
}

export default FiltersControls;
