#!/usr/bin/env node

import { chdir } from "process";

chdir(__dirname);

import fs from "fs";
import { ethers } from "hardhat";
import { z } from "zod";
import { fieldToHex } from "../utils/mina";

const schema = z.object({
  address: z.string(),
  previousLocation: z.string(),
  sourceReceiptChainHashes: z.string(),
  targetSparseLedger: z.string(),
  sigData: z.array(z.string().transform(fieldToHex)),
  commands: z.array(z.string().transform((data) => Buffer.from(data, "base64"))),
});

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  const {
    address,
    previousLocation,
    sourceReceiptChainHashes,
    targetSparseLedger,
    sigData,
    commands,
  } = schema.parse(JSON.parse(fs.readFileSync(0).toString()));

  const contract = daFactory.attach(address);

  try {
    const tx = await contract.postBatch(
      previousLocation,
      sourceReceiptChainHashes,
      commands,
      targetSparseLedger,
      sigData
    );
    await tx.wait();
  } catch (e) {
    if (
      e instanceof Object &&
      e.hasOwnProperty("reason") &&
      // @ts-ignore
      e.reason.includes("Batch already exists")
    ) {
      process.exit(0);
    }
    console.error(e);
    process.exit(1);
  }
};

main().catch(console.error);
