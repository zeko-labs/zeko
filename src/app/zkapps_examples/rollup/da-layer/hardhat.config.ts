import "@nomicfoundation/hardhat-toolbox";
import { HardhatUserConfig } from "hardhat/config";

const TESTING_PRIVATE_KEY = "";

const config: HardhatUserConfig = {
  solidity: {
    version: "0.8.18",
    settings: {
      optimizer: {
        enabled: true,
        runs: 1000,
      },
    },
  },

  defaultNetwork: "dev",
  networks: {
    dev: {
      url: process.env.DA_PROVIDER ?? "http://localhost:8545",
      accounts: [process.env.DA_PRIVATE_KEY ?? TESTING_PRIVATE_KEY],
    },
  },
};

export default config;
