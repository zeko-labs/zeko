import { minaExplorerUrl, zekoExplorerUrl } from "./constants";

export async function timeout(seconds: number): Promise<void> {
  return new Promise<void>((resolve) => {
    setTimeout(() => {
      resolve();
    }, seconds * 1000);
  });
}

export const getMinaExplorerTxUrl = (hash: string) => {
  return `${minaExplorerUrl}/transaction/${hash}`;
};

export const getZekoExplorerTxUrl = (hash: string) => {
  return `${zekoExplorerUrl}/tx/${hash}`;
};
