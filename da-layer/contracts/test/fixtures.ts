import { ethers } from "hardhat";
import { PublicKey } from "snarkyjs";
import { DataAvailabilityLayer } from "../typechain-types";
import { Proxy } from "../typechain-types/contracts/Multisig";
import { createCombinedArtifact } from "../utils/abi";
import { fieldToHex } from "../utils/field";

export type DataAvailabilityUpgradeable = DataAvailabilityLayer & Proxy;

export const deployDataAvailabilityContract = async (validators: PublicKey[]) => {
  const upgradeableArtifact = await createCombinedArtifact(
    "contracts/Multisig/Proxy.sol:Proxy",
    "DataAvailabilityLayer"
  );

  const implementationFactory = await ethers.getContractFactory("DataAvailabilityLayer");
  const proxyFactory = await ethers.getContractFactoryFromArtifact(upgradeableArtifact);

  const implementation = await implementationFactory.deploy();

  await implementation.deployed();

  const proxy = await proxyFactory.deploy(
    implementation.address,
    validators.length,
    validators.map((validator) => ({
      x: fieldToHex(validator.toGroup().x),
      y: fieldToHex(validator.toGroup().y),
    }))
  );

  return proxy as DataAvailabilityUpgradeable;
};
