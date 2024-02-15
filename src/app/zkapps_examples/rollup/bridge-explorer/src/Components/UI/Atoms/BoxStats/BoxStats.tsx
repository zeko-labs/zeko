import { BoxColor } from "../../../../Constants";
import Loader from "../Loader";

type BoxStatsProps = {
  text: string;
  value?: string;
  color?: BoxColor.PRIMARY | BoxColor.DEFAULT;
  img?: JSX.Element;
  isLoading?: boolean;
  isFetching?: boolean;
};

const BoxStats = ({
  isLoading = false,
  text,
  value,
  color = BoxColor.DEFAULT,
  img,
}: BoxStatsProps) => {
  return (
    <div className="bg-gradient-secondary h-[98px] rounded-md py-6 px-4 shadow-default">
      <p className="text-sm font-bold text-white">{text}</p>
      <div className="flex items-baseline justify-between">
        {isLoading ? (
          <span
            className={`mt-2 ${color === BoxColor.PRIMARY ? "text-orange-800" : "text-gray-200"}`}
          >
            <Loader className="h-5" />
          </span>
        ) : (
          <span
            className={`text-3xl ${
              color === BoxColor.PRIMARY ? "text-orange-800" : "text-gray-200"
            }`}
          >
            {value}
          </span>
        )}
        <div>{img}</div>
      </div>
    </div>
  );
};

export default BoxStats;
