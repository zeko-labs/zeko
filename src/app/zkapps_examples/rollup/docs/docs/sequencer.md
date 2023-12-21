# Zeko sequencer

## Build

```bash
DUNE_PROFILE=devnet dune build
```

## Tests

```bash
# Run lightnet to imitate L1
zk lightnet start

DUNE_PROFILE=devnet dune runtest
```

## Run

```bash
DUNE_PROFILE=devnet dune exec ./run.exe -- \
    -p <int?> \
    --rest-server <string> \
    --zkapp-pk <string> \
    --signer <string> \
    --max-pool-size <int?> \
    --commitment-period <float?> \
    --da-contract-address <string?> \
    --db-dir <string?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```

## Deploy

```bash
DUNE_PROFILE=devnet dune exec ./deploy.exe -- \
    --rest-server <string> \
    --init-state <int> \
    --signer <string>
```

Run help to see the options:

```bash
dune exec ./deploy.exe -- --help
```
