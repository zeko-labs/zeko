import { ZodType, z } from "zod";

const processInt = (schema: ZodType) =>
  z.preprocess((x: unknown) => {
    if (typeof x === "string") return parseInt(x, 10);
    else x;
  }, schema);

const processBool = (schema: ZodType) =>
  z.preprocess((x: unknown) => {
    if (typeof x === "string") return x === "true";
    else x;
  }, schema);

const schema = z.object({
  PORT: processInt(z.number().default(4000)),
  DA_LAYER_WEBSOCKET_URL: z.string().default("ws://localhost:8546"),
  DA_LAYER_PRIVATE_KEY: z.string().default("0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2"),
  DA_LAYER_CONTRACT_ADDRESS: z.string().default("0xD28E88C8C016f97f4087bC48E947d5EB6226C87f"),
  GENESIS_ACCOUNTS_PATH: z.string().default("genesis-accounts.json"),
  COMMITMENT_PERIOD: processInt(z.number().default(120_000)),
  WS_KEEP_ALIVE_INTERVAL: processInt(z.number().default(7_500)),
  WS_EXPECTED_PONG_BACK: processInt(z.number().default(15_000)),
  MINA_SIGNER_PRIVATE_KEY: z.string().default(""),
  MINA_ZKAPP_PRIVATE_KEY: z.string().default(""),
  MINA_NODE_URL: z.string().default(""),
  MAX_MEMPOOL_SIZE: processInt(z.number().default(5)),
});

export default schema.parse(process.env);
