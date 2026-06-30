# Verification key upgrade with Docker multisig

Use this when `multisig_key` changed in `ZEKO_CIRCUITS_CONFIG` and the inner
and outer verification keys must be upgraded through the multisig branch.

The important split is:

- Generate and sign upgrade bodies with the new config. This computes the new
  verification keys.
- Submit/prove with the config that matches the currently installed on-chain
  verification key. For a transition from old multisig key to new multisig key,
  this means the final submit before the outer VK upgrade uses the old config
  and signatures from the old multisig signers.

Do not use `--direct` for the multisig flow.

## Inputs

Set these on the machine coordinating the upgrade:

```bash
export IMAGE=zekolabs/zeko:v1.0.5
export L1_URI='https://api.minascan.io/node/mainnet/v1/graphql'
export DA_NODE='mainnet-da-node-1.zeko.io:1924'

export OLD_CONFIG="$PWD/configs/mainnet v1.json"
export NEW_CONFIG="$PWD/configs/mainnet.json"

export WORK="$PWD/configs/vk-upgrade-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$WORK/signatures" "$WORK/bodies" "$WORK/inner-db"
```

If the L1 or DA endpoint is on the Docker host, use the host address that works
from inside the container, for example `host.docker.internal` on Docker Desktop.

The image must contain `/bin/zeko-cli`. The Nix `zeko-image` does; override the
entrypoint because its default entrypoint runs the sequencer.

## Optional checks

Check which outer VKs differ under the new config:

```bash
docker run --rm \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-outer-verification-keys \
  --l1-uri "$L1_URI" \
  --only-check
```

Check which inner VKs differ. This command needs any real private key because
the CLI derives a public key for the state query.

```bash
export MINA_PRIVATE_KEY='<any-private-key-for-state-query>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-inner-verification-keys \
  --l1-uri "$L1_URI" \
  --da-node "$DA_NODE" \
  --db-path /work/inner-db \
  --only-check
```

## 1. Prepare the inner VK update once

Run this once with one old multisig signer. It syncs the current L2 ledger from
DA, writes the new inner VKs into a local ledger, distributes the DA diff, and
produces one signed outer-state update.

```bash
export SIGNER_NAME='<coordinator-name>'
export MINA_PRIVATE_KEY='<old-multisig-signer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-inner-verification-keys \
  --l1-uri "$L1_URI" \
  --da-node "$DA_NODE" \
  --db-path /work/inner-db \
  --output "/work/signatures/inner-$SIGNER_NAME.json"
```

Extract the body for the remaining old multisig signers:

```bash
docker run --rm \
  -v "$WORK:/work" \
  --entrypoint /bin/jq \
  "$IMAGE" \
  '.[0].body' \
  "/work/signatures/inner-$SIGNER_NAME.json" \
  > "$WORK/bodies/inner-body.json"
```

Each remaining old multisig signer signs exactly that body:

```bash
export SIGNER_NAME='<signer-name>'
export MINA_PRIVATE_KEY='<old-multisig-signer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  sign-multisig-update \
  --kind Outer \
  --body-file /work/bodies/inner-body.json \
  --output "/work/signatures/inner-$SIGNER_NAME.json"
```

## 2. Sign the outer VK updates

Each old multisig signer runs this with the new config. It may produce several
signed updates: the core outer account, L1 bridge holder accounts, and the L1
helper token owner account.

```bash
export SIGNER_NAME='<signer-name>'
export MINA_PRIVATE_KEY='<old-multisig-signer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-outer-verification-keys \
  --l1-uri "$L1_URI" \
  --output "/work/signatures/outer-$SIGNER_NAME.json"
```

Collect at least the old quorum for every payload before submitting. The submit
command groups signatures by payload, so it is fine that each `outer-*.json`
contains multiple signed updates.

## 3. Submit inner first with the old config

Submit the inner ledger-hash update before changing the outer VK. This keeps the
proof config and signer set aligned with the current outer VK.

```bash
export MINA_PRIVATE_KEY='<fee-payer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/old.json \
  -v "$OLD_CONFIG:/configs/old.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  multisig-submit \
  --l1-uri "$L1_URI" \
  --signed-update-file /work/signatures/inner-SIGNER_1.json \
  --signed-update-file /work/signatures/inner-SIGNER_2.json
```

Wait until the transaction is accepted/final enough for your operational
threshold before continuing.

## 4. Submit outer last with the old config

This installs the new outer, bridge holder, and helper token owner VKs. After
this succeeds, future multisig submissions must use the new config and the new
multisig signer set.

```bash
export MINA_PRIVATE_KEY='<fee-payer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/old.json \
  -v "$OLD_CONFIG:/configs/old.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  multisig-submit \
  --l1-uri "$L1_URI" \
  --signed-update-file /work/signatures/outer-SIGNER_1.json \
  --signed-update-file /work/signatures/outer-SIGNER_2.json
```

## 5. Verify with the new config

```bash
docker run --rm \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-outer-verification-keys \
  --l1-uri "$L1_URI" \
  --only-check
```

```bash
export MINA_PRIVATE_KEY='<any-private-key-for-state-query>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-inner-verification-keys \
  --l1-uri "$L1_URI" \
  --da-node "$DA_NODE" \
  --db-path /work/inner-db \
  --only-check
```

Both checks should report `equal: true` for the relevant VKs.

## If the DA signer set also changed

Changing `multisig_key` changes the governance multisig baked into the VKs.
Changing the DA committee used for commits is a separate outer-state update:

```bash
export SIGNER_NAME='<new-signer-name>'
export MINA_PRIVATE_KEY='<current-multisig-signer-private-key>'

docker run --rm \
  -e MINA_PRIVATE_KEY \
  -e ZEKO_CIRCUITS_CONFIG=/configs/new.json \
  -v "$NEW_CONFIG:/configs/new.json:ro" \
  -v "$WORK:/work" \
  --entrypoint /bin/zeko-cli \
  "$IMAGE" \
  update-da-key \
  --l1-uri "$L1_URI" \
  --da-key '<da-public-key-1>' \
  --da-key '<da-public-key-2>' \
  --quorum '<da-quorum>' \
  --output "/work/signatures/da-key-$SIGNER_NAME.json"
```

Collect the current quorum, then submit it with `multisig-submit` using the
config that matches the current outer VK at the time of submission.
