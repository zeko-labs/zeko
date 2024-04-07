#!/usr/bin/env node

import fs from "fs";
import { ethers } from "hardhat";
import { z } from "zod";
import { DataAvailability } from "../typechain-types";
import { MinaCommandStruct } from "../typechain-types/contracts/DataAvailability";
import { fieldToHex } from "../utils/mina";

const schema = z.object({
  address: z.string(),
  from: z
    .string()
    .optional()
    .transform((id) => id && fieldToHex(id)),
  to: z
    .string()
    .optional()
    .transform((id) => id && fieldToHex(id)),
});

const getTransactions = async (
  contract: DataAvailability,
  from: string,
  to: string
): Promise<MinaCommandStruct[]> => {
  if (to === from || to === ethers.constants.HashZero) {
    return [];
  }

  const [genesis, previousId, commands] = await contract.getBatchData(to);

  if (genesis) {
    return commands;
  }

  return [...(await getTransactions(contract, from, previousId)), ...commands];
};

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  let { address, from, to } = schema.parse(JSON.parse(fs.readFileSync(0).toString()));

  const contract = daFactory.attach(address);

  if (from === undefined) {
    from = ethers.constants.HashZero;
  }

  if (to === undefined) {
    to = await contract.lastBatchId();
  }

  const commands = await getTransactions(contract, from, to);

  console.log(
    JSON.stringify(
      commands.map(({ commandType, data }) => ({
        commandType,
        // @ts-ignore
        data: Buffer.from(ethers.utils.arrayify(data)).toString("base64"),
      })),
      null,
      2
    )
  );
};

main().catch(console.error);
