import { Link } from "react-router-dom";
import { useAppProvider } from "../../../context/AppContext";

export default function NotFound() {
  const { selectedNetwork } = useAppProvider();
  return (
    <div className="px-8 pt-40 text-center text-white">
      <h1 className="text-4xl">404</h1>
      <p className="my-4">We couldn't find this page</p>
      <Link to={`/${selectedNetwork.id}`} className="primary-btn">
        Go Home
      </Link>
    </div>
  );
}
