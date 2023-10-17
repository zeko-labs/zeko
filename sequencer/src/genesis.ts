import fs from "fs/promises";
import { MinaUtils, MlPublicKey } from "o1js";
import { z } from "zod";
import config from "./config";

export type GenesisAccount = {
  publicKey: MlPublicKey;
  balance: string;
};

const genesisAccountsSchema = z.array(
  z.object({
    publicKey: z.string(),
    balance: z.number(),
  })
);

export const loadAccounts = async (): Promise<GenesisAccount[]> => {
  const genesisAccounts = await fs.readFile(config.GENESIS_ACCOUNTS_PATH, "utf-8");
  const accounts = JSON.parse(genesisAccounts);

  const parsedAccounts = genesisAccountsSchema.parse(accounts);

  return parsedAccounts.map(({ publicKey, balance }) => ({
    publicKey: MinaUtils.encoding.publicKeyOfBase58(publicKey),
    balance: balance.toString(),
  }));
};
