import { ethers } from "hardhat";
import { PublicKey } from "snarkyjs";
import { DataAvailabilityProxy } from "../typechain-types";
import { DataAvailabilityDev } from "../typechain-types/contracts/dev";
import { createCombinedArtifact } from "../utils/abi";
import { fieldToHex } from "../utils/mina";

export type DataAvailabilityUpgradeable = DataAvailabilityDev & DataAvailabilityProxy;

export const deployDataAvailabilityContract = async (
  validators: PublicKey[],
  quorum?: number,
  sequencerAddress?: string
) => {
  const [sequencer] = await ethers.getSigners();

  const upgradeableArtifact = await createCombinedArtifact(
    "DataAvailabilityProxy",
    "DataAvailabilityDev"
  );

  const implementationFactory = await ethers.getContractFactory("DataAvailabilityDev");
  const proxyFactory = await ethers.getContractFactoryFromArtifact(upgradeableArtifact);

  const implementation = await implementationFactory.deploy();

  await implementation.deployed();

  const proxy = await proxyFactory.deploy(
    implementation.address,
    quorum ?? validators.length,
    validators.map((validator) => ({
      x: fieldToHex(validator.toGroup().x),
      y: fieldToHex(validator.toGroup().y),
    })),
    sequencerAddress ?? sequencer.address
  );

  return proxy as DataAvailabilityUpgradeable;
};
