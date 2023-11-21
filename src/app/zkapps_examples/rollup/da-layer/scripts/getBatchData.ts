import fs from "fs";
import { ethers } from "hardhat";
import { Field } from "o1js";
import { z } from "zod";
import { fieldToHex } from "../utils/mina";

const schema = z.object({
  address: z.string(),
  id: z.string().transform((id) => fieldToHex(Field(id))),
});

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  const { address, id } = schema.parse(JSON.parse(fs.readFileSync(0).toString()));

  const contract = daFactory.attach(address);

  const [_, commands] = await contract.getBatchData(id);

  console.log(
    JSON.stringify(
      commands.map(({ commandType, data }) => ({
        commandType,
        data: Buffer.from(ethers.utils.arrayify(data)).toString("base64"),
      })),
      null,
      2
    )
  );
};

main().catch(console.error);
