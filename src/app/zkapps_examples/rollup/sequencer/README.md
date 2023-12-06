# Zeko sequencer

## Build

```bash
DUNE_PROFILE=devnet dune build
```

## Run

```bash
dune exec ./run.exe -- \
    -p <int?> \
    --zkapp-pk <string> \
    --max-pool-size <int?> \
    --commitment-period <float?> \
    --da-contract-address <string?> \
    --db-dir <string?>
```

Run help to see the options:

```bash
dune exec ./run.exe -- --help
```
