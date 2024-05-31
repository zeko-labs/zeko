# Mina Testing Ledger

This is a local ledger for testing Mina transaction logic. It's not a full network and therefore it's much lighter and faster than using the [lightnet](https://docs.minaprotocol.com/zkapps/writing-a-zkapp/introduction-to-zkapps/testing-zkapps-lightnet).

It's just a local ledger with database and graphql API exposing few usefull mutations and queries.

## Usage

There are 2 possible modes:

1. **Transaction pool mode**
   - the ledger creates also a transaction pool like normal node and periodically (based on `--block-period`) applies all the transactions from the pool to the ledger.
   - alternatively you can use `createNewBlock` mutation to apply the transactions from pool manually.
   - it's useful for testing of transaction pool.
2. **Instant mode**
   - the ledger applies the transactions immediately after they are submitted.

There is also custom mutation `createAccount` which accepts `publicKey` to create new account with `1_000_000_000_000` nanomina.

### Options

- `-p` - port for the graphql API, default is `8080`.
- `--db-dir` - directory where the ledger database will be stored, default is `./l1_db`, if you want temporary ledger you can use `/tmp/...` directory.
- `--block-period` - period in seconds for creating new block, if not provided the ledger will be in instant mode.
- `--genesis-account` - base58 public key of the genesis account with some funds

```bash
DUNE_PROFILE=devnet dune exec ./tests/local_network/run.exe -- -p 8080 --db-dir l1_db --block-period 5 --genesis-account B62...
```
