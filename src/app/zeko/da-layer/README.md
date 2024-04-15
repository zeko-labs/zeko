How to run:

```bash
geth --config geth.toml --datadir <datadir> --unlock 0xd78c987031b0256c9ea6289185e151c25efd0e36 --mine --keystore ./geth_db/keystore/ --http --allow-insecure-unlock --password /dev/null init genesis.json
geth --config geth.toml --datadir <datadir> --unlock 0xd78c987031b0256c9ea6289185e151c25efd0e36 --mine --keystore ./geth_db/keystore/ --http --allow-insecure-unlock --password /dev/null
```

If you forget to init the database geth will implicitly init it incorrectly.

The specified account's private key is included in the keystore which is checked in to git.
This doesn't matter given that this geth instance is private anyway.

Run `deploy` script to deploy contract to geth network specified by URI in `DA_PROVIDER` environment variable,
or `127.0.0.1:8545` by default. It uses IPv4 explicitly because seemingly hardhat fucks up when it sees `localhost` with
both IPv4 and IPv6 entries in the host file.

The scripts `postBatch` and `getBatches` take their arguments as JSON on stdin, connect to geth,
do their thing, and return on stdout.
