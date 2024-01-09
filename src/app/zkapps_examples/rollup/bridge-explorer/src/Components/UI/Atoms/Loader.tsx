import { ReactComponent as Spinner } from "../../../Assets/spinner.svg";

export default function Loader({ className = "" }: { className?: string }) {
  return <Spinner className={`h-4 animate-spin ${className}`} />;
}
