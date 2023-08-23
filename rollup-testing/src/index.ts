import Client from "mina-signer";
import {
  AccountUpdate,
  Async_js,
  Mina,
  MinaUtils,
  PrivateKey,
  RollupBindings,
  Signature,
  Types,
  UserCommand,
  prettifyStacktracePromise,
  withThreadPool,
} from "snarkyjs";
import { MessagePort, Worker, isMainThread, parentPort } from "worker_threads";

const client = new Client({ network: "testnet" });

const zkappPrivateKey = PrivateKey.fromBase58("EKEGtxdWZHAX6FLo9hxiLeJ8to1yGLnYZDYr2j9QUt3Mc7bHgqgW");

const privKey = "EKEGtxdWZHAX6FLo9hxiLeJ8to1yGLnYZDYr2j9QUt3Mc7bHgqgW";
const pubKey = client.derivePublicKey(privKey);
const toPubKey = "B62qm8mVEkhAZdnnoE4gzKhTYj2Gre88GNhoknbJdgDrE8JQojNBgWa";

type MessageType = "prove" | "commit" | "start" | "ready" | "exit";

type WorkerMessage = {
  type: MessageType;
  data: string;
};

async function waitForMessage(port: Worker | MessagePort, type: MessageType): Promise<WorkerMessage> {
  return new Promise<WorkerMessage>((resolve) => {
    port.on("message", (msg: WorkerMessage) => {
      if (msg.type === type) {
        resolve(msg);
      }
    });
  });
}

function generateCommand(nonce: number): UserCommand {
  const { signature, data } = client.signPayment(
    {
      to: toPubKey,
      from: pubKey,
      amount: 50_000_000_000,
      fee: 10,
      nonce,
      validUntil: Math.floor(+new Date() / 1000) + 1000,
      memo: "",
    },
    privKey
  );

  return {
    signature: Signature.fromJSON({
      r: signature.field,
      s: signature.scalar,
    }).toBase58(),
    fromBase58: pubKey,
    toBase58: toPubKey,
    amount: data.amount.toString(),
    fee: data.fee.toString(),
    validUntil: data.validUntil!.toString(),
    nonce: data.nonce.toString(),
    memo: MinaUtils.encoding.memoToBase58(data.memo?.toString() ?? ""),
  };
}

Async_js.init();

if (isMainThread) {
  prettifyStacktracePromise(withThreadPool(masterMain)).catch(console.error);
} else {
  prettifyStacktracePromise(withThreadPool(workerMain)).catch(console.error);
}

async function masterMain() {
  const worker = new Worker(__filename, { workerData: { isSnarkyMainThread: true } });

  await waitForMessage(worker, "start");

  const Local = Mina.LocalBlockchain({
    proofsEnabled: true,
  });
  Mina.setActiveInstance(Local);

  const { privateKey: senderKey, publicKey: senderAddress } = Local.testAccounts[0];

  console.log("Compiling master...");
  const { applyUserCommand, createZkapp } = RollupBindings.compile(
    MinaUtils.encoding.publicKeyOfBase58(zkappPrivateKey.toPublicKey().toBase58())
  );

  await waitForMessage(worker, "ready");

  console.log("all compiled");

  console.log("Creating zkapp...");
  const { accountUpdate: deployAccountUpdate, rollup } = createZkapp("rollup", [
    {
      publicKey: MinaUtils.encoding.publicKeyOfBase58("B62qnPZzpnQWA8FLBn9qqJqPTeGuDdHTZgpmEMUNFCq8fWCRSqJS6Jd"),
      balance: "1000000000000",
    },
  ]);

  console.log("Deploying...");
  const deployTx = await Mina.transaction(senderAddress, () => {
    AccountUpdate.fundNewAccount(senderAddress);
  });

  const snarkyjsDeployAccountUpdate = AccountUpdate.fromJSON(JSON.parse(deployAccountUpdate));
  snarkyjsDeployAccountUpdate.lazyAuthorization = { kind: "lazy-signature" };

  deployTx.transaction.accountUpdates.push(snarkyjsDeployAccountUpdate);

  await deployTx.prove();
  await deployTx.sign([senderKey, zkappPrivateKey]).send();

  const command1 = generateCommand(0);

  console.log("Applying command...");

  const { txnSnarkInputJson } = applyUserCommand(rollup, command1);

  worker.postMessage({
    type: "prove",
    data: txnSnarkInputJson,
  });

  const txnSnark = await new Promise<string>((resolve) => {
    worker.on("message", (msg: WorkerMessage) => {
      if (msg.type === "prove") {
        resolve(msg.data);
      }
    });
  });

  worker.postMessage({
    type: "commit",
    data: txnSnark,
  });

  const stepCallForest = await new Promise<string>((resolve) => {
    worker.on("message", (msg: WorkerMessage) => {
      if (msg.type === "commit") {
        resolve(msg.data);
      }
    });
  });

  console.log("Updating zkapp...");
  const stepTx = await Mina.transaction(senderAddress, () => {});

  const stepAccountUpdates = (JSON.parse(stepCallForest) as Types.Json.AccountUpdate[]).map((au) =>
    AccountUpdate.fromJSON(au)
  );

  stepTx.transaction.accountUpdates.push(...stepAccountUpdates);

  await stepTx.sign([senderKey, zkappPrivateKey]).send();

  worker.postMessage({
    type: "exit",
  });
}

async function workerMain() {
  parentPort!.postMessage({
    type: "start",
  });

  console.log("Compiling worker...");
  const { commit, proveUserCommand } = RollupBindings.compile(
    MinaUtils.encoding.publicKeyOfBase58(zkappPrivateKey.toPublicKey().toBase58())
  );

  parentPort!.postMessage({
    type: "ready",
  });

  parentPort!.on("message", async (msg: WorkerMessage) => {
    if (msg.type === "prove") {
      try {
        console.log("Proving command...");
        proveUserCommand(msg.data, "", (next) => {
          parentPort!.postMessage({
            type: "prove",
            data: next,
          });
        });
      } catch (e) {
        console.error(e);
      }
    }

    if (msg.type === "commit") {
      try {
        console.log("Committing...");
        commit(msg.data, (accountUpdate) => {
          parentPort!.postMessage({
            type: "commit",
            data: accountUpdate,
          });
        });
      } catch (e) {
        console.error(e);
      }
    }
  });

  await waitForMessage(parentPort!, "exit");
}
