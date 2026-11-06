# Bootstrap role entrypoints

Scripts under `bin/` are the one-shot Zeko half of fresh-rollup bootstrapping:
fresh circuit/deploy configuration, signer TLS material, the bootstrap L1
stand-in and the first proof + genesis + public VK export. They run from the
same `zekolabs/zeko` image as the production stack. The Ethereum half (address
prediction, guest build, program vkeys, contract deployment) lives in the
`ethereum-settlement` repository's `zeko-bootstrap` image.

The signer, DA node and prover are **not** bootstrap roles. They are ordinary
long-lived services started before the one-shots and kept running afterwards
for the L2; they use the images' native entrypoints (`/bin/zeko-signer`,
`/bin/zeko-da`, `/bin/zeko-prover`), see "Long-lived services" below. The
`zekolabs/zeko-da` image carries no bootstrap scripts.

`nix/docker.nix` copies the scripts into `zekolabs/zeko` with the image bash as
shebang (`mkBootstrapRoles`). The image keeps its normal entrypoint
(`/bin/zeko-run`); a role is selected with a Compose `entrypoint:` override
plus `command:` arguments.

## Role to image mapping

| Script (in `/bin`) | Image | Wraps | Arguments |
|---|---|---|---|
| `zeko-bootstrap-generate-config` | `zekolabs/zeko` | `zeko-cli generate-circuits-config` | `<0x bridge proxy address> [output-dir]` (default `/run/zeko/config`) |
| `zeko-bootstrap-signer-tls` | `zekolabs/zeko` | `openssl req` | none; env `ZEKO_SIGNER_TLS_HOSTNAME`, optional `ZEKO_SIGNER_TLS_DIR`, `ZEKO_SIGNER_TLS_DAYS` |
| `zeko-bootstrap-l1` | `zekolabs/zeko` | `zeko-testing-ledger` (bootstrap L1 stand-in) | `<port>` |
| `zeko-bootstrap-export` | `zekolabs/zeko` | `zeko-deployment-export` + `jq` post-processing | none; env, see script header |

`zeko-bootstrap-common.sh` is sourced by the others; it is not a role.

## Contract (one-shot roles)

- Secrets are files: the exporter reads `/run/secrets/signer_auth_token` into
  `ZEKO_SIGNER_AUTH_TOKEN` and never prints it.
- `ZEKO_CIRCUITS_CONFIG` and `ZEKO_DEPLOY_CONFIG` must name mounted operator
  files; `test` (embedded test keys) is refused by the exporter.
- `RABBITMQ_USER` / `RABBITMQ_PASSWORD` must be a dedicated broker user, not
  `guest`.
- `ZEKO_SIGNER_TLS_CA_FILE` (default `/run/zeko/signer-tls/ca/signer.crt`) is
  the certificate signer clients verify against `ZEKO_SIGNER_TLS_HOSTNAME`.
- Writable state goes under `ZEKO_BOOTSTRAP_STATE_DIR` (default
  `/var/lib/zeko-bootstrap`): `l1/`, `sequencer/`. Bind-mount it per role; the
  image declares no volume for it.
- `ZEKO_BOOTSTRAP_WORKSPACE_HOST` must be the absolute host path of the
  operator workspace, outside every source checkout; the guard refuses
  checkout-shaped paths.
- Generated private material (`circuits-config.json`, `deploy-config.json`,
  `tls/private/signer.key`) is written with `umask 077`; run the containers as
  the workspace owner's uid:gid (Compose `user:`) so the files stay readable
  only to that user.
- The exporter writes into an empty mounted `ZEKO_BOOTSTRAP_OUTPUT_DIR`
  (default `/out`): `settlement.json`, `deployment-manifest.json`,
  `genesis-ledger.json`, `vk.serde.json`, `settlement-vk.serde.json`,
  `proof.serde.json`, `public_input_skeleton.json`, `app_statement.json`.
  Set `ZEKO_BOOTSTRAP_SOURCE_REVISION` to the zeko commit of the image so the
  manifest records it (the image does not know its own revision).

Per-role `entrypoint:` / `command:` pairs:

| Compose service | Image | `entrypoint:` | `command:` |
|---|---|---|---|
| `generate-config` (one-shot) | `zekolabs/zeko` | `["/bin/zeko-bootstrap-generate-config"]` | `["0x<bridge proxy address>"]` (via `docker compose run --rm generate-config 0x…`) |
| `signer-tls` (one-shot) | `zekolabs/zeko` | `["/bin/zeko-bootstrap-signer-tls"]` | `[]` |
| `bootstrap-l1` (one-shot profile, runs for the export) | `zekolabs/zeko` | `["/bin/zeko-bootstrap-l1"]` | `["8080"]` |
| `exporter` (one-shot) | `zekolabs/zeko` | `["/bin/zeko-bootstrap-export"]` | `[]`; env `ZEKO_BOOTSTRAP_L1=bootstrap-l1:8080`, `ZEKO_BOOTSTRAP_MQ=rabbitmq:5672`, `ZEKO_BOOTSTRAP_SIGNER=sequencer-signer:8600`, `ZEKO_BOOTSTRAP_DA_NODES=da1:8555`, `ZEKO_BOOTSTRAP_DA_QUORUM=1`, Postgres host/password or URI |

The exporter waits for each endpoint (L1 stand-in, broker, signer, DA nodes)
before starting. Its `db-dir` state (`state/exporter/sequencer`) and the DA
node's database are the state the L2 continues from; keep them with the export
output.

## Long-lived services

The sequencer signer, DA signer, DA node and prover are started once, serve the
export, and keep running for the L2 (the sequencer and gateway join them later).
They are plain Compose services without a profile and without wrapper scripts.
What the native binaries need, verified against `src/app/zeko`:

| Binary | Command line | Reads from the environment |
|---|---|---|
| `zeko-signer` (`signer/cli.ml`) | `run --host 0.0.0.0 --port <p> --allow-zkapp-signing --max-fee <mina> --max-balance-change <mina> --tls-cert-file <crt> --tls-key-file <key>` (sequencer signer) or `run --host 0.0.0.0 --port <p> --allow-field-signing --tls-cert-file <crt> --tls-key-file <key>` (DA signer) | `MINA_PRIVATE_KEY` (Base58 private key), `ZEKO_SIGNER_AUTH_TOKEN` (shared bearer token) |
| `zeko-da` (`da_layer/cli.ml`) | `run-node --port <p> --healthcheck-port <h> --network-id <id> --db-dir <dir> --signer <host:port>` | `ZEKO_SIGNER_AUTH_TOKEN`; `ZEKO_SIGNER_TLS_CA_FILE` + `ZEKO_SIGNER_TLS_HOSTNAME` (TLS to the signer, `signer_service.ml` `Tls.Client_config.of_env`) |
| `zeko-prover` (`sequencer/prover/cli.ml`) | `run-server --mq-host <host:port>` | `ZEKO_CIRCUITS_CONFIG`, `ZEKO_DEPLOY_CONFIG` (operator config files; unset or `test` selects embedded test keys, so always set them), `RABBITMQ_USER`, `RABBITMQ_PASSWORD` (`message_queue.ml`; not `guest`, which the broker limits to loopback) |

Facts that shape the service definitions:

- `zeko-signer run` defaults to `--host localhost`; a container must pass
  `--host 0.0.0.0`, which the signer accepts only with TLS configured (or
  `--allow-insecure-remote-binding`, not used here).
- The binaries read the private key and the auth token from environment
  variables, not files. Compose file secrets arrive as files under
  `/run/secrets`, and Compose cannot map a file into an environment variable
  on its own, so the signer and DA services use a documented
  `entrypoint: ["/bin/bash", "-c", …]` one-liner that reads the secret file
  into the variable and `exec`s the binary. The `--private-key` /
  `--auth-token` flags are not used: flag values are visible in `docker
  inspect` and the process list. The read happens inside the container;
  nothing is logged.
- `zeko-da run-node` fetches the signer's public key once at startup
  (`Signer_service.Client.create`, one try) and exits if the signer is not
  reachable, so the DA node depends on a healthy signer. The signer has no
  health endpoint; its healthcheck is a pure-bash LISTEN probe on
  `/proc/net/tcp` (no connection is opened, so the TLS server stays quiet).
  The DA node serves `GET /health` on `--healthcheck-port` (`node.ml`), probed
  with the image's `curl`.
- `zeko-prover` compiles the circuits (`Compile_circuits.compile_all`) before
  `Message_queue.Worker.start`, so an ESTABLISHED connection from the prover
  container to the broker port exists only once the jobs consumer is
  registering. The healthcheck is a pure-bash probe of `/proc/net/tcp` for
  state `01` and remote port `1628` (hex of 5672); only bash is required.
- Compose interpolates `$`, so `$$` is written where the shell must see `$`.
  Ports: sequencer signer 8600 (`0x2198`), DA signer 8601 (`0x2199`), DA node
  8555 RPC / 8558 health, broker 5672 (`0x1628`).

Service definitions (same security model as the one-shots: workspace owner's
`user:`, file secrets, `read_only`, `cap_drop: [ALL]`, internal network;
`ZEKO_SIGNER_TLS_HOSTNAME`, `ZEKO_SIGNER_TLS_CA_FILE` and
`MINA_SIGNING_NETWORK_ID` come from the shared environment):

```yaml
sequencer-signer:
  image: docker.io/zekolabs/zeko:<release-tag>
  user: "${ZEKO_BOOTSTRAP_UID}:${ZEKO_BOOTSTRAP_GID}"
  restart: unless-stopped
  entrypoint:
    - /bin/bash
    - -c
    - |
      set -euo pipefail
      export MINA_PRIVATE_KEY=$$(</run/secrets/sequencer_key)
      export ZEKO_SIGNER_AUTH_TOKEN=$$(</run/secrets/signer_auth_token)
      [[ -n $$MINA_PRIVATE_KEY && -n $$ZEKO_SIGNER_AUTH_TOKEN ]] || { echo "empty secret file" >&2; exit 1; }
      exec /bin/zeko-signer run --host 0.0.0.0 --port 8600 --allow-zkapp-signing \
        --max-fee 10 --max-balance-change 1000000 \
        --tls-cert-file /run/zeko/signer-tls/ca/signer.crt \
        --tls-key-file /run/zeko/signer-tls/private/signer.key
  healthcheck:
    test: ["CMD", "/bin/bash", "-c", "while read -r _ local _ st _; do [[ $$st == 0A && $$local == *:2198 ]] && exit 0; done </proc/net/tcp; exit 1"]
    interval: 10s
    retries: 30
  secrets: [sequencer_key, signer_auth_token]
  volumes:
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/tls/ca:/run/zeko/signer-tls/ca:ro
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/tls/private:/run/zeko/signer-tls/private:ro

da1-signer:
  image: docker.io/zekolabs/zeko-da:<release-tag>
  # as sequencer-signer, with /run/secrets/da1_key, --port 8601,
  # --allow-field-signing (no --max-fee/--max-balance-change) and the
  # healthcheck pattern *:2199

da1:
  image: docker.io/zekolabs/zeko-da:<release-tag>
  user: "${ZEKO_BOOTSTRAP_UID}:${ZEKO_BOOTSTRAP_GID}"
  restart: unless-stopped
  entrypoint:
    - /bin/bash
    - -c
    - |
      set -euo pipefail
      export ZEKO_SIGNER_AUTH_TOKEN=$$(</run/secrets/signer_auth_token)
      [[ -n $$ZEKO_SIGNER_AUTH_TOKEN ]] || { echo "empty secret file" >&2; exit 1; }
      exec /bin/zeko-da run-node --port 8555 --healthcheck-port 8558 \
        --network-id "$$MINA_SIGNING_NETWORK_ID" \
        --db-dir /var/lib/zeko-bootstrap/da --signer da1-signer:8601
  healthcheck:
    test: ["CMD", "curl", "-fsS", "http://127.0.0.1:8558/health"]
    interval: 10s
    retries: 30
  secrets: [signer_auth_token]
  volumes:
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/tls/ca:/run/zeko/signer-tls/ca:ro
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/state/da1:/var/lib/zeko-bootstrap
  depends_on:
    da1-signer:
      condition: service_healthy

prover:
  image: docker.io/zekolabs/zeko:<release-tag>
  user: "${ZEKO_BOOTSTRAP_UID}:${ZEKO_BOOTSTRAP_GID}"
  restart: unless-stopped
  entrypoint: ["/bin/zeko-prover"]
  command: ["run-server", "--mq-host", "rabbitmq:5672"]
  environment:
    ZEKO_CIRCUITS_CONFIG: /run/zeko/config/circuits-config.json
    ZEKO_DEPLOY_CONFIG: /run/zeko/config/deploy-config.json
    RABBITMQ_USER: ${ZEKO_BOOTSTRAP_RABBITMQ_USER}
    RABBITMQ_PASSWORD: ${ZEKO_BOOTSTRAP_RABBITMQ_PASSWORD}
  volumes:
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/config:/run/zeko/config:ro
    - ${ZEKO_BOOTSTRAP_WORKSPACE}/state/prover:/var/lib/zeko-bootstrap
  # Circuit compilation can take hours on proving-grade hardware: failures
  # during start_period do not count.
  healthcheck:
    test: ["CMD", "/bin/bash", "-c", "for f in /proc/net/tcp /proc/net/tcp6; do [[ -r $$f ]] || continue; while read -r _ _ rem st _; do [[ $$st == 01 && $$rem == *:1628 ]] && exit 0; done <$$f; done; exit 1"]
    interval: 30s
    timeout: 10s
    retries: 10
    start_period: 8h
  depends_on:
    rabbitmq:
      condition: service_healthy
```

The broker credentials reach the prover as plain environment variables from the
operator's `.env` (as in the exporter); the broker has no file-secret interface
in Zeko. The prover does not fail closed on a missing `ZEKO_CIRCUITS_CONFIG`
the way the exporter does: the Compose must set both config variables
explicitly, which the definition above does.

Postgres (`postgres:16`) and RabbitMQ (`rabbitmq:latest`) are ordinary
containers on the same network. The full example that combines these services
with the one-shots is `ethereum-settlement/nix/bootstrap/compose.yaml`.
