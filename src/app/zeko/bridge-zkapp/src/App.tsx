import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { MuiSetup } from "./MuiSetup";
import Navbar from "./components/Navbar";
import HomePage from "./pages/HomePage";
import "./reactCOIServiceWorker";

const queryClient = new QueryClient();

function Layout() {
  return (
    <QueryClientProvider client={queryClient}>
      <MuiSetup>
        <Navbar />
        <HomePage />
      </MuiSetup>
    </QueryClientProvider>
  );
}

function App() {
  return <Layout />;
}

export default App;
