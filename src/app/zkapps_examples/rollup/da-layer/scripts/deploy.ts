import { ethers } from "hardhat";
import { PrivateKey } from "o1js";
import validatorsKeys from "../test-validators.json";
import { createCombinedArtifact } from "../utils/abi";
import { fieldToHex } from "../utils/mina";

const main = async () => {
  const upgradeableArtifact = await createCombinedArtifact(
    "DataAvailabilityProxy",
    "DataAvailability"
  );

  const implementationFactory = await ethers.getContractFactory("DataAvailability");
  const proxyFactory = await ethers.getContractFactoryFromArtifact(upgradeableArtifact);

  const implementation = await implementationFactory.deploy();

  await implementation.deployed();

  const validators = validatorsKeys.map(PrivateKey.fromBase58);
  const quorum = Math.floor(validators.length / 2 + 1);

  const proxy = await proxyFactory.deploy(
    implementation.address,
    quorum,
    validators.map((validator) => {
      const { x, y } = validator.toPublicKey().toGroup();
      return {
        x: fieldToHex(x),
        y: fieldToHex(y),
      };
    }),
    "0xd78C987031B0256C9EA6289185E151C25EFD0e36"
  );

  console.log(proxy.address);
};

main().catch(console.error);
