import { QueryClient, QueryClientProvider } from "react-query";
import { BrowserRouter, Navigate, Route, Routes, useLocation } from "react-router-dom";
import Home from "./Pages/Home/Home";
import Search from "./Pages/Search/Search";
import TxDetail from "./Pages/TxDetail";
import NotFound from "./Components/UI/Molecules/NotFound";
import { Fragment, useLayoutEffect } from "react";
import { Networks, networksMap } from "./Services/ConfigService";
import { AppProvider } from "./context/AppContext";
import MainLayout from "./Components/layout/MainLayout";

export const networkList = Object.values(networksMap || {});
export const initialNetwork = networksMap[Networks.mainnet];

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      cacheTime: 1000 * 60 * 60 * 1, // 24 hours
    },
  },
});

const ScrollToTop = () => {
  const location = useLocation();
  useLayoutEffect(() => {
    document.documentElement.scrollTo(0, 0);
  }, [location.pathname]);
  return null;
};

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <AppProvider>
        <BrowserRouter>
          <ScrollToTop />
          <MainLayout>
            <Routes>
              {/* By default redirect to mainnet */}
              <Route path="/" element={<Navigate to={`/${initialNetwork.id}`} replace />} />
              {networkList.map((network) => (
                <Fragment key={network.id}>
                  <Route path={`/${network.id}`} element={<Home />} />
                  <Route path={`/${network.id}/search`} element={<Search />} />
                  <Route path={`/${network.id}/search/:type`} element={<Search />} />
                  <Route path={`/${network.id}/tx/:id`} element={<TxDetail />} />
                </Fragment>
              ))}
              <Route path="*" element={<NotFound />} />
            </Routes>
          </MainLayout>
        </BrowserRouter>
      </AppProvider>
    </QueryClientProvider>
  );
}

export default App;
