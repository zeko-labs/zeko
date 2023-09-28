import { ApolloServer } from "@apollo/server";
import assert from "assert";
import { ethers } from "ethers";
import { Field, Mina, MinaUtils, PrivateKey, PublicKey, Signature } from "snarkyjs";
import { Account, Maybe, Mutation } from "../src/generated/graphql";
import { RollupContext, loadSchema, resolvers } from "../src/gql";
import { CommandType, Rollup, createRollupContext } from "../src/rollup";
import { bigIntStrToHex, decimalToMina, minaToDecimal } from "../src/utils";
import { generateCommand } from "./utils";

describe("Payment", () => {
  let server: ApolloServer<RollupContext>;
  let context: RollupContext;
  let newContext: RollupContext;

  const acc1 = ((sk) => ({ sk, pk: sk.toPublicKey() }))(PrivateKey.random());
  const acc2 = ((sk) => ({ sk, pk: sk.toPublicKey() }))(PrivateKey.random());
  const zkapp = ((sk) => ({ sk, pk: sk.toPublicKey() }))(PrivateKey.random());
  let signer: { sk: PrivateKey; pk: PublicKey };

  const payment1 = generateCommand(acc1.sk, acc2.pk, minaToDecimal(10), 0);
  const payment2 = generateCommand(acc1.sk, acc2.pk, minaToDecimal(20), 1);
  const payment3 = generateCommand(acc2.sk, acc1.pk, minaToDecimal(15), 0);

  let genesisLedgerHash: Field;

  beforeAll(async () => {
    const Local = Mina.LocalBlockchain({
      proofsEnabled: true,
      enforceTransactionLimits: true,
    });
    Mina.setActiveInstance(Local);

    signer = {
      sk: Local.testAccounts[0].privateKey,
      pk: Local.testAccounts[0].publicKey,
    };

    context = await createRollupContext(
      [
        {
          balance: minaToDecimal(1_000).toString(),
          publicKey: MinaUtils.encoding.publicKeyOfBase58(acc1.pk.toBase58()),
        },
      ],
      signer.sk,
      zkapp.sk
    );

    server = new ApolloServer<RollupContext>({
      typeDefs: await loadSchema("schema.graphql"),
      resolvers,
    });
  }, 1_000_000);

  afterAll(async () => {
    await context.teardown();
  }, 100_000);

  it("deploys the zkapp", async () => {
    await context.rollup.deploy();

    const zkappAcc = Mina.getAccount(zkapp.pk);

    const committedLedgerHash = zkappAcc.zkapp?.appState.at(0);

    expect(committedLedgerHash).toBeDefined();
    assert(committedLedgerHash !== undefined, "zkapp not deployed");

    const realLedgerHash = context.rollup.getRoot();

    expect(committedLedgerHash.equals(realLedgerHash).toBoolean()).toBe(true);

    genesisLedgerHash = committedLedgerHash;
  });

  test("only genesis account in ledger", async () => {
    const { body } = await server.executeOperation<{ acc1: Maybe<Account>; acc2: Maybe<Account> }>(
      {
        query: `
          query {
            acc1: account(publicKey: ${acc1.pk.toBase58()}) {
              balance { total }
              nonce
            }
            acc2: account(publicKey: ${acc2.pk.toBase58()}) {
              balance { total }
              nonce
            }
          }
        `,
      },
      { contextValue: context }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.acc1).toMatchObject({
      balance: { total: minaToDecimal(1_000).toString() },
      nonce: "0",
    });
    expect(body.singleResult.data?.acc2).toBe(null);
  });

  it("should apply first payment", async () => {
    const { body } = await server.executeOperation<Pick<Mutation, "sendPayment">>(
      {
        query: `
          mutation {
            sendPayment(
              signature: {
                field: "${payment1.signature.field}",
                scalar: "${payment1.signature.scalar}",
              },
              input: {
                nonce: "${payment1.data.nonce.toString()}",
                memo: "${payment1.data.memo ?? ""}",
                validUntil: "${payment1.data.validUntil?.toString()}",
                fee: "${payment1.data.fee.toString()}",
                amount: "${payment1.data.amount.toString()}",
                to: "${acc2.pk.toBase58()}",
                from: "${acc1.pk.toBase58()}",
              }
            ) {
              payment {
                nonce
              }
            }
          }
        `,
      },
      { contextValue: context }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.sendPayment).toMatchObject({
      payment: { nonce: 0 },
    });
  });

  it("should have both accounts in ledger", async () => {
    const { body } = await server.executeOperation<{ acc1: Maybe<Account>; acc2: Maybe<Account> }>(
      {
        query: `
          query {
            acc1: account(publicKey: ${acc1.pk.toBase58()}) {
              balance { total }
              nonce
            }
            acc2: account(publicKey: ${acc2.pk.toBase58()}) {
              balance { total }
              nonce
            }
          }
        `,
      },
      { contextValue: context }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.acc1).toMatchObject({
      balance: {
        total: (minaToDecimal(990) - 10).toString(),
      },
      nonce: "1",
    });
    expect(body.singleResult.data?.acc2).toMatchObject({
      balance: { total: (minaToDecimal(10) - minaToDecimal(1, 6)).toString() },
      nonce: "0",
    });
  });

  it("should have posted the first command to the DA layer", async () => {
    const jsonPayment = JSON.stringify({
      payload: {
        common: {
          fee: decimalToMina(+payment1.data.fee.toString()).toFixed(20),
          fee_payer_pk: payment1.publicKey,
          nonce: payment1.data.nonce.toString(),
          valid_until: ["Since_genesis", payment1.data.validUntil?.toString() ?? "0"],
          memo: MinaUtils.encoding.memoToBase58(payment1.data.memo?.toString() ?? ""),
        },
        body: [
          "Payment",
          {
            receiver_pk: payment1.data.to,
            amount: payment1.data.amount.toString(),
          },
        ],
      },
      signer: payment1.publicKey,
      signature: Signature.fromJSON({
        r: payment1.signature.field,
        s: payment1.signature.scalar,
      }).toBase58(),
    });

    const expectedTxId = MinaUtils.transactionHash.paymentToBase64(jsonPayment);

    const storedPayment = await context.rollup.daLayer.contract.getCommandData(0);

    expect(storedPayment.commandType).toBe(CommandType.SignedCommand);
    expect(Buffer.from(storedPayment.data.slice(2), "hex").toString("base64")).toBe(expectedTxId);
  });

  it("should apply second payment", async () => {
    const { body } = await server.executeOperation<Pick<Mutation, "sendPayment">>(
      {
        query: `
          mutation {
            sendPayment(
              signature: {
                field: "${payment2.signature.field}",
                scalar: "${payment2.signature.scalar}",
              },
              input: {
                nonce: "${payment2.data.nonce.toString()}",
                memo: "${payment2.data.memo ?? ""}",
                validUntil: "${payment2.data.validUntil?.toString()}",
                fee: "${payment2.data.fee.toString()}",
                amount: "${payment2.data.amount.toString()}",
                to: "${acc2.pk.toBase58()}",
                from: "${acc1.pk.toBase58()}",
              }
            ) {
              payment {
                nonce
              }
            }
          }
        `,
      },
      { contextValue: context }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.sendPayment).toMatchObject({
      payment: { nonce: 1 },
    });
  });

  it("should have posted the second command to the DA layer", async () => {
    const jsonPayment = JSON.stringify({
      payload: {
        common: {
          fee: decimalToMina(+payment2.data.fee.toString()).toFixed(20),
          fee_payer_pk: payment2.publicKey,
          nonce: payment2.data.nonce.toString(),
          valid_until: ["Since_genesis", payment2.data.validUntil?.toString() ?? "0"],
          memo: MinaUtils.encoding.memoToBase58(payment2.data.memo?.toString() ?? ""),
        },
        body: [
          "Payment",
          {
            receiver_pk: payment2.data.to,
            amount: payment2.data.amount.toString(),
          },
        ],
      },
      signer: payment2.publicKey,
      signature: Signature.fromJSON({
        r: payment2.signature.field,
        s: payment2.signature.scalar,
      }).toBase58(),
    });

    const expectedTxId = MinaUtils.transactionHash.paymentToBase64(jsonPayment);

    const storedPayment = await context.rollup.daLayer.contract.getCommandData(1);

    expect(storedPayment.commandType).toBe(CommandType.SignedCommand);
    expect(Buffer.from(storedPayment.data.slice(2), "hex").toString("base64")).toBe(expectedTxId);
  });

  it("should commit the state to L1", async () => {
    await context.rollup.txnSnarkPromise;

    expect(context.rollup.lastTxnSnark).not.toBe("");
    expect(context.rollup.stagedTransactions.length).toBe(2);
    expect(context.rollup.committedTransactions.length).toBe(0);

    await context.rollup.commit();

    expect(context.rollup.stagedTransactions.length).toBe(0);
    expect(context.rollup.committedTransactions.length).toBe(2);

    const zkappAcc = Mina.getAccount(zkapp.pk);

    const committedLedgerHash = zkappAcc.zkapp?.appState.at(0);

    expect(committedLedgerHash).toBeDefined();
    assert(committedLedgerHash !== undefined, "zkapp not deployed");

    const realLedgerHash = context.rollup.getRoot();

    expect(committedLedgerHash.equals(realLedgerHash).toBoolean()).toBe(true);
    expect(committedLedgerHash.equals(genesisLedgerHash).toBoolean()).toBe(false);
  }, 500_000);

  it("should have posted the batch ordering to the DA layer", async () => {
    const batchId = bigIntStrToHex(context.rollup.getRoot().toString());
    const [storedPreviousBatchId, ordering] = await context.rollup.daLayer.contract.getBatchData(batchId);

    expect(storedPreviousBatchId).toBe(ethers.constants.HashZero);
    expect(ordering.map((x) => x.toNumber())).toEqual([0, 1]);
  });

  it("should bootstrap new rollup context", async () => {
    newContext = {
      rollup: new Rollup(zkapp.sk, signer.sk, context.rollup.daLayer, context.rollup.bindings, [
        {
          balance: minaToDecimal(1_000).toString(),
          publicKey: MinaUtils.encoding.publicKeyOfBase58(acc1.pk.toBase58()),
        },
      ]),
      teardown: async () => {},
    };

    await newContext.rollup.bootstrap(context.rollup.lastCommittedBatchId);

    expect(newContext.rollup.lastCommittedBatchId).toBe(context.rollup.lastCommittedBatchId);

    const expectedLedgerHash = context.rollup.getRoot();
    const actualLedgerHash = newContext.rollup.getRoot();

    expect(expectedLedgerHash.equals(actualLedgerHash).toBoolean()).toBe(true);
  });

  it("should apply third payment to the new rollup context", async () => {
    const { body } = await server.executeOperation<Pick<Mutation, "sendPayment">>(
      {
        query: `
          mutation {
            sendPayment(
              signature: {
                field: "${payment3.signature.field}",
                scalar: "${payment3.signature.scalar}",
              },
              input: {
                nonce: "${payment3.data.nonce.toString()}",
                memo: "${payment3.data.memo ?? ""}",
                validUntil: "${payment3.data.validUntil?.toString()}",
                fee: "${payment3.data.fee.toString()}",
                amount: "${payment3.data.amount.toString()}",
                to: "${acc1.pk.toBase58()}",
                from: "${acc2.pk.toBase58()}",
              }
            ) {
              payment {
                nonce
              }
            }
          }
        `,
      },
      { contextValue: newContext }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.sendPayment).toMatchObject({
      payment: { nonce: 0 },
    });
  });

  it("should have correct accounts in ledger", async () => {
    const { body } = await server.executeOperation<{ acc1: Maybe<Account>; acc2: Maybe<Account> }>(
      {
        query: `
          query {
            acc1: account(publicKey: ${acc1.pk.toBase58()}) {
              balance { total }
              nonce
            }
            acc2: account(publicKey: ${acc2.pk.toBase58()}) {
              balance { total }
              nonce
            }
          }
        `,
      },
      { contextValue: newContext }
    );

    assert(body.kind === "single");

    expect(body.singleResult.data?.acc1).toMatchObject({
      balance: {
        total: (minaToDecimal(985) - 20).toString(),
      },
      nonce: "2",
    });
    expect(body.singleResult.data?.acc2).toMatchObject({
      balance: { total: (minaToDecimal(15) - minaToDecimal(1, 6) - 10).toString() },
      nonce: "1",
    });
  });

  it("should have posted the third command to the DA layer", async () => {
    const jsonPayment = JSON.stringify({
      payload: {
        common: {
          fee: decimalToMina(+payment3.data.fee.toString()).toFixed(20),
          fee_payer_pk: payment3.publicKey,
          nonce: payment3.data.nonce.toString(),
          valid_until: ["Since_genesis", payment3.data.validUntil?.toString() ?? "0"],
          memo: MinaUtils.encoding.memoToBase58(payment3.data.memo?.toString() ?? ""),
        },
        body: [
          "Payment",
          {
            receiver_pk: payment3.data.to,
            amount: payment3.data.amount.toString(),
          },
        ],
      },
      signer: payment3.publicKey,
      signature: Signature.fromJSON({
        r: payment3.signature.field,
        s: payment3.signature.scalar,
      }).toBase58(),
    });

    const expectedTxId = MinaUtils.transactionHash.paymentToBase64(jsonPayment);

    const storedPayment = await newContext.rollup.daLayer.contract.getCommandData(2);

    expect(storedPayment.commandType).toBe(CommandType.SignedCommand);
    expect(Buffer.from(storedPayment.data.slice(2), "hex").toString("base64")).toBe(expectedTxId);
  });

  it("should commit the second batch to L1", async () => {
    await newContext.rollup.txnSnarkPromise;

    expect(newContext.rollup.lastTxnSnark).not.toBe("");
    expect(newContext.rollup.stagedTransactions.length).toBe(1);
    expect(newContext.rollup.committedTransactions.length).toBe(2);

    await newContext.rollup.commit();

    expect(newContext.rollup.stagedTransactions.length).toBe(0);
    expect(newContext.rollup.committedTransactions.length).toBe(3);

    const zkappAcc = Mina.getAccount(zkapp.pk);

    const committedLedgerHash = zkappAcc.zkapp?.appState.at(0);

    expect(committedLedgerHash).toBeDefined();
    assert(committedLedgerHash !== undefined, "zkapp not deployed");

    const realLedgerHash = newContext.rollup.getRoot();

    expect(committedLedgerHash.equals(realLedgerHash).toBoolean()).toBe(true);
    expect(committedLedgerHash.equals(context.rollup.getRoot()).toBoolean()).toBe(false);
  }, 500_000);

  it("should have posted the second batch ordering to the DA layer", async () => {
    const batchId = bigIntStrToHex(newContext.rollup.getRoot().toString());
    const [storedPreviousBatchId, ordering] = await newContext.rollup.daLayer.contract.getBatchData(batchId);

    expect(storedPreviousBatchId).toBe(context.rollup.lastCommittedBatchId);
    expect(ordering.map((x) => x.toNumber())).toEqual([2]);
  });
});
