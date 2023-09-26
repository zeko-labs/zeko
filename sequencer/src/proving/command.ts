import { ProvingWorker } from "./worker";

export const commandProver = new ProvingWorker(
  __filename,
  (bindings, input: { snarkInp: string; prevSnark: string | undefined }) => {
    return new Promise<string>((resolve) => {
      bindings.proveUserCommand(input.snarkInp, input.prevSnark, (nextSnark) => {
        resolve(nextSnark);
      });
    });
  }
);

commandProver.workerMain();
