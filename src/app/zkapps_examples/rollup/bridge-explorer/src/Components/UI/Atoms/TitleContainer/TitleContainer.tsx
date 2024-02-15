import React from "react";
import { useIsFetching } from "react-query";
import { SearchInput } from "../../Molecules/FiltersControls/FiltersControls";
import Loader from "../Loader";

type TitleContainerProps = {
  text: string;
  onSearchClicked: (e: React.FormEvent<HTMLFormElement>) => void;
  value: string;
  setValue: (e: string) => void;
};

const TitleContainer = ({ text, onSearchClicked, value, setValue }: TitleContainerProps) => {
  const isFetching = useIsFetching();

  return (
    <div className="my-6 flex w-full flex-col justify-between md:flex-row md:items-center">
      <div className="flex items-center">
        <h2 className="text-lg font-bold text-white">{text}</h2>
        {Boolean(isFetching) ? <Loader className="ml-3 text-gray-200" /> : null}
      </div>
      <form className="flex flex-col items-center md:flex-row" onSubmit={onSearchClicked}>
        <SearchInput
          containerClassName="mt-3 md:mt-0 md:mr-3 w-full md:w-auto"
          placeholder="Tx ID..."
          label="Search:"
          setValue={setValue}
          value={value}
          id="search"
          hideLabel
        />
        <button type="submit" className="primary-btn mt-4 w-full md:mt-0 md:w-auto">
          Search
        </button>
      </form>
    </div>
  );
};

export default TitleContainer;
