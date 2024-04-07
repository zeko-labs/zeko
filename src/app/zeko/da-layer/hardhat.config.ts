import "@nomicfoundation/hardhat-toolbox";
import { HardhatUserConfig, subtask } from "hardhat/config";
import "@typechain/hardhat";

const TESTING_PRIVATE_KEY = "0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2";

const {
  TASK_COMPILE_SOLIDITY_GET_SOLC_BUILD,
} = require("hardhat/builtin-tasks/task-names");

if (process.env.SOLC_COMPILER_PATH) {
  subtask(
    TASK_COMPILE_SOLIDITY_GET_SOLC_BUILD,
    async (
      args: {
        solcVersion: string;
      },
      hre,
      runSuper
    ) => {
      return {
        compilerPath: process.env.SOLC_COMPILER_PATH,
        isSolcJs: false,
        version: process.env.SOLC_COMPILER_VERSION,
        longVersion: process.env.SOLC_COMPILER_VERSION,
      };
    }
  );
}

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
  typechain: {
    outDir: "./typechain-types",
    target: "ethers-v5"
  },
  defaultNetwork: "dev",
  networks: {
    dev: {
      url: process.env.DA_PROVIDER ?? "http://127.0.0.1:8545",
      accounts: [TESTING_PRIVATE_KEY],
    },
  },
};

export default config;
