import fs from "fs";
import { ethers } from "hardhat";
import { Field } from "o1js";
import { fieldToHex } from "../utils/mina";

const main = async () => {
  const daFactory = await ethers.getContractFactory("DataAvailability");

  const [address, id] = fs.readFileSync(0).toString().trim().split(" ");

  const contract = daFactory.attach(address);

  const [_, commands] = await contract.getBatchData(fieldToHex(Field(id)));

  commands.forEach(({ commandType, data }) =>
    console.log(commandType, Buffer.from(ethers.utils.arrayify(data)).toString("base64"))
  );
};

main().catch(console.error);
