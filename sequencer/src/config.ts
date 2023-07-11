import { z } from "zod";

const schema = z.object({
  PORT: z.number().default(4000),
  DA_LAYER_WEBSOCKET_URL: z.string().default("ws://localhost:8546"),
  DA_LAYER_PRIVATE_KEY: z.string().default("0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2"),
  DA_LAYER_CONTRACT_ADDRESS: z.string().default("0xD28E88C8C016f97f4087bC48E947d5EB6226C87f"),
  GENESIS_ACCOUNTS_PATH: z.string().default("genesis-accounts.json"),
});

export default schema.parse(process.env);
