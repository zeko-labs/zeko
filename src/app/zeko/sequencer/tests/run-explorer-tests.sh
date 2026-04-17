#!/usr/bin/env bash
set -euo pipefail

CONTAINER_NAME="nats-sequencer-explorer-tests"
NATS_URL="${NATS_URL:-nats://127.0.0.1:4222}"

cleanup() {
  docker rm -f "${CONTAINER_NAME}" >/dev/null 2>&1 || true
}

trap cleanup EXIT

wait_for_port() {
  local port="$1"
  local attempts=30
  while ! nc -z 127.0.0.1 "${port}" >/dev/null 2>&1; do
    attempts=$((attempts - 1))
    if [ "${attempts}" -le 0 ]; then
      echo "Timed out waiting for port ${port}" >&2
      exit 1
    fi
    sleep 1
  done
}

cleanup

docker run --rm --name "${CONTAINER_NAME}" -p 4222:4222 -d nats:2-alpine >/dev/null
wait_for_port 4222

opam exec -- env -u DUNE_RPC dune runtest --profile=devnet \
  src/app/zeko/sequencer/explorer/tests
NATS_URL="${NATS_URL}" opam exec -- env -u DUNE_RPC dune exec --profile=devnet \
  src/app/zeko/sequencer/explorer/tests/explorer_nats_gherkin_tests.exe
