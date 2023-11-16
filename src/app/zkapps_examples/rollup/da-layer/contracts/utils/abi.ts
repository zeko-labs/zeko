import { artifacts } from "hardhat";
import { Artifact } from "hardhat/types";

export interface AbiEntry {
  name: string;
  [key: string]: unknown;
}

export type Abi = AbiEntry[];

export const unifyAbi = (a: Abi, b: Abi) => {
  const entryNames = new Set([...a, ...b].map(({ name }) => name));

  return Array.from(entryNames).map((distinctName) =>
    [...a, ...b].find(({ name }) => name === distinctName)
  );
};

export const createCombinedArtifact = async (
  proxyName: string,
  implementationName: string
): Promise<Artifact> => {
  const [proxy, implementation] = await Promise.all([
    artifacts.readArtifact(proxyName),
    artifacts.readArtifact(implementationName),
  ]);

  return { ...proxy, abi: unifyAbi(proxy.abi, implementation.abi) };
};
