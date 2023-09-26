import { Async_js, MlPublicKey, RollupBindings, RollupMethods, withThreadPool } from "snarkyjs";
import { MessagePort, Worker, isMainThread, parentPort, workerData } from "worker_threads";
import logger from "../logger";

type MessageToParentType = "started" | "ready" | "exited" | "done";
type MessageToWorkerType = "exit" | "work";

export class ProvingWorker<
  InputT,
  OutputT,
  MessageToParent extends
    | {
        type: MessageToParentType;
        data?: OutputT;
      }
    | {
        type: "error";
        error: Error;
      },
  MessageToWorker extends {
    type: MessageToWorkerType;
    data?: InputT;
  }
> {
  public readyPromise: Promise<any> | null = null;
  public startedPromise: Promise<any> | null = null;
  public isReady: boolean = false;

  private worker: Worker | null = null;
  private bindings: RollupMethods | null = null;

  private jobsQueue: {
    resolve: (data: OutputT) => void;
    reject: (err: Error) => void;
    input: InputT;
  }[] = [];
  private isProcessing: boolean = false;

  constructor(private fileName: string, private fn: (bindings: RollupMethods, input: InputT) => Promise<OutputT>) {}

  public async enqueue(input: InputT): Promise<OutputT> {
    if (!this.isReady) {
      throw new Error("worker is not ready");
    }

    return new Promise<OutputT>((resolve, reject) => {
      this.jobsQueue.push({ resolve, reject, input });
      this.processQueue();
    });
  }

  private processQueue() {
    if (this.isProcessing || this.jobsQueue.length === 0 || this.worker === null) return;

    const { input, resolve, reject } = this.jobsQueue.shift()!;

    this.isProcessing = true;

    this.worker.once("message", (message: MessageToParent) => {
      this.isProcessing = false;

      if (message.type === "done" && message.data !== undefined) {
        logger.debug(`Worker finished job with status: ${message.type}`);
        resolve(message.data);
      } else if (message.type === "error") {
        logger.debug(`Worker finished job with status: ${message}`);
        reject(message.error);
      }

      this.processQueue();
    });

    this.worker.postMessage({ type: "work", data: input } as MessageToWorker);
  }

  public async stop() {
    if (!isMainThread) return;
    if (this.worker === null) return;

    const exitedPromise = this.waitForMessage(this.worker, "exited");
    this.worker.postMessage({ type: "exit" } as MessageToWorker);

    await exitedPromise;

    this.worker.terminate();

    logger.debug("Worker terminated");

    this.worker = null;
    this.readyPromise = null;
    this.isReady = false;
    this.bindings = null;
  }

  public start(zkappPk: MlPublicKey) {
    if (!isMainThread) return;

    this.worker = new Worker(this.fileName, { workerData: { isSnarkyMainThread: true, zkappPk } });

    this.startedPromise = this.waitForMessage(this.worker, "started");
    this.readyPromise = this.waitForMessage(this.worker, "ready");

    this.readyPromise.then(() => {
      this.isReady = true;
    });
  }

  public workerMain() {
    if (isMainThread) return;

    withThreadPool(async () => {
      parentPort!.postMessage({ type: "started" } as MessageToParent);

      Async_js.init();

      logger.debug("compiling rollup bindings");
      this.bindings = RollupBindings.compile(workerData.zkappPk as MlPublicKey);
      logger.debug("bindings ready");

      parentPort!.postMessage({ type: "ready" } as MessageToParent);

      await new Promise<void>((resolve) => {
        parentPort!.on("message", async (msg: MessageToWorker) => {
          try {
            if (msg.type === "work") {
              const { data } = msg;

              if (data === undefined) throw new Error("data is undefined");
              if (this.bindings === null) throw new Error("bindings is null");

              const result = await this.fn(this.bindings, data);

              parentPort!.postMessage({ type: "done", data: result } as MessageToParent);
            }
          } catch (err) {
            parentPort!.postMessage({ type: "error", error: err } as MessageToParent);
          }

          if (msg.type === "exit") {
            logger.debug("Worker exitting");
            resolve();
          }
        });
      });
    })
      .then(() => parentPort!.postMessage({ type: "exited" } as MessageToParent))
      .catch(logger.error);
  }

  private async waitForMessage(port: Worker | MessagePort, type: MessageToParentType | MessageToWorkerType) {
    return new Promise<void>((resolve) => {
      port.on("message", (msg: MessageToWorker | MessageToParent) => {
        if (msg.type === type) {
          resolve();
        }
      });
    });
  }
}
