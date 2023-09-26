import { ProvingWorker } from "./worker";

export const commandProver = new ProvingWorker(
  __filename,
  (bindings, input: { snarkInp: string; prevSnark: string }) => {
    return new Promise<string>((resolve) => {
      console.log("Proving command...");
      bindings.proveUserCommand(input.snarkInp, input.prevSnark, (nextSnark) => {
        console.log("Done proving command");
        resolve(nextSnark);
      });
    });
  }
);

commandProver.workerMain();
