import fs from "fs";
import { ethers } from "hardhat";
import { Field } from "o1js";
import { fieldToHex } from "../utils/mina";

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  const [address, ...rest] = fs
    .readFileSync(0)
    .toString()
    .trim()
    .split("\n")
    .map((x) => x.trim());

  const contract = daFactory.attach(address);

  const [[rawId, rawPreviousId], ...rawCommands] = rest.map((x) => x.split(" "));

  const id = fieldToHex(Field(rawId));
  const previousId = fieldToHex(Field(rawPreviousId));

  const commands = rawCommands.map(([commandTypeStr, data]) => {
    const commandType = parseInt(commandTypeStr);

    if (commandType < 0 || commandType > 2) {
      throw new Error("Invalid command type");
    }

    return {
      commandType,
      data: Buffer.from(data, "base64"),
    };
  });

  const tx = await contract.postBatch(id, previousId, commands);

  await tx.wait();
};

main().catch(console.error);
