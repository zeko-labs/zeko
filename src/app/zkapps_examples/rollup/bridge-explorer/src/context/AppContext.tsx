import * as React from "react";
import { initialNetwork, queryClient } from "../App";
import { Network, Networks, networksMap } from "../Services/ConfigService";

type AppContextType = {
  selectedNetwork: Network;
  setSelectedNetwork: (value: Network) => void;
};

const AppContext = React.createContext({} as AppContextType);

const AppProvider = ({ children }: { children: React.ReactNode }) => {
  const [selectedNetwork, setSelectedNetwork] = React.useState(() => {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const [_, network] = window.location.pathname.split("/");
    const isValidNetwork = Object.keys(networksMap).find((item) => item === network);
    return isValidNetwork ? networksMap[network as Networks] : initialNetwork;
  });

  React.useEffect(() => {
    queryClient.refetchQueries();
    queryClient.resetQueries();
    queryClient.clear();
  }, [selectedNetwork]);

  return (
    <AppContext.Provider
      value={{
        selectedNetwork,
        setSelectedNetwork,
      }}
    >
      {children}
    </AppContext.Provider>
  );
};

const useAppProvider = () => {
  const context = React.useContext(AppContext);
  if (!context) {
    throw new Error("useAppProvider must be used within a AppProvider");
  }
  return context;
};

export { useAppProvider, AppProvider };
