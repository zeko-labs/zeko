import "@nomicfoundation/hardhat-toolbox";
import { HardhatUserConfig } from "hardhat/config";

const TESTING_PRIVATE_KEY = "0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2";

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
    testnet: {
      url: "https://evm-rpc-zeko-dev.dcspark.io/",
      accounts: [process.env.DA_PRIVATE_KEY ?? TESTING_PRIVATE_KEY],
    },
  },
};

export default config;
