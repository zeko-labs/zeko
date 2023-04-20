import "@nomicfoundation/hardhat-toolbox";
import { HardhatUserConfig } from "hardhat/config";

const TESTING_PRIVATE_KEY = "0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2";

const config: HardhatUserConfig = {
  solidity: "0.8.18",
  networks: {
    dev: {
      url: "http://localhost:8545",
      accounts: [TESTING_PRIVATE_KEY],
    },
  },
};

export default config;
