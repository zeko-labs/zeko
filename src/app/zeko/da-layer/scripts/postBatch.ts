import fs from "fs";
import { ethers } from "hardhat";
import { z } from "zod";
import { fieldToHex } from "../utils/mina";

const schema = z.object({
  address: z.string(),
  id: z.string().transform(fieldToHex),
  previousId: z.string().transform(fieldToHex),
  commands: z.array(
    z.object({
      commandType: z.number().int().gte(0).lte(1),
      data: z.string().transform((data) => Buffer.from(data, "base64")),
    })
  ),
});

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  const { address, id, previousId, commands } = schema.parse(
    JSON.parse(fs.readFileSync(0).toString())
  );

  const contract = daFactory.attach(address);

  try {
    const tx = await contract.postBatch(id, previousId, commands);
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
