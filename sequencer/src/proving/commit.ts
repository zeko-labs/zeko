import { ProvingWorker } from "./worker";

export const commitProver = new ProvingWorker(__filename, (bindings, input: { txnSnark: string }) => {
  return new Promise<string>((resolve) => {
    bindings.commit(input.txnSnark, (accountUpdate) => {
      resolve(accountUpdate);
    });
  });
});

commitProver.workerMain();
