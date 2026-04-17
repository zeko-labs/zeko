#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <fake|real> <num_provers>" >&2
  exit 1
}

if [ "$#" -ne 2 ]; then
  usage
fi

MODE="$1"
NUM_PROVERS="$2"
PROVER_PIDS=()
SIGNER_PIDS=()
PROVERS=()

case "$MODE" in
fake | real) ;;
*)
  echo "Error: first argument must be 'fake' or 'real'" >&2
  usage
  ;;
esac

if ! [[ "$NUM_PROVERS" =~ ^[1-9][0-9]*$ ]]; then
  echo "Error: second argument must be a positive integer" >&2
  usage
fi

cleanup() {
  local exit_status=$1
  echo "Cleaning up..."
  kill ${l1_pid:-} ${da1_pid:-} ${da2_pid:-} ${da3_pid:-} "${PROVER_PIDS[@]}" "${SIGNER_PIDS[@]}" 2>/dev/null
  rm -rf "$TMP_DIR"
  docker rm -f pg-sequencer 2>/dev/null
  docker rm -f rabbitmq-sequencer 2>/dev/null
  exit ${exit_status:-0}
}

trap 'cleanup 1' SIGINT SIGTERM
trap 'cleanup $?' EXIT

SEQUENCER_ROOT="$(git rev-parse --show-toplevel)/src/app/zeko/sequencer"
SEQUENCER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/sequencer"
SIGNER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/signer"

opam exec --switch . -- env -u DUNE_RPC dune build \
  src/app/zeko/sequencer/tests/testing_ledger/run.exe \
  src/app/zeko/da_layer/cli.exe \
  src/app/zeko/sequencer/prover/cli.exe \
  src/app/zeko/sequencer/prover/cli_fake.exe \
  src/app/zeko/sequencer/tests/sequencer_test.exe \
  src/app/zeko/sequencer/tests/sequencer_test_fake.exe

export ZEKO_SIGNATURE_KIND=testnet
export ZEKO_CIRCUITS_CONFIG=test

TMP_DIR=$(mktemp -d)

wait_for_port() {
  local port=$1
  local pid=$2
  while ! nc -z localhost $port; do
    sleep 1

    if ! kill -0 $pid 2>/dev/null; then
      echo "Process for port $port failed to start"
      exit 1
    fi
  done

  echo "Port $port is now open"
}

KEYGEN_BIN="$SEQUENCER_BUILD_ROOT/cli.exe"

generate_even_key() {
  "$KEYGEN_BIN" generate-even-key | awk -F': ' '/Private key:/ {print $2}'
}

docker run --rm --name pg-sequencer \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid \
  -p 5433:5432 \
  -d postgres:16-alpine

docker run -d --name rabbitmq-sequencer \
  -p 5672:5672 \
  rabbitmq:latest

wait_for_port 5433 $$
wait_for_port 5672 $$

SEQUENCER_SIGNER_BIN="$SIGNER_BUILD_ROOT/cli.exe"
DA_SIGNER_BIN="$SIGNER_BUILD_ROOT/cli.exe"

ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY="$(generate_even_key)"
DA1_SIGNER_PRIVATE_KEY="$(generate_even_key)"
DA2_SIGNER_PRIVATE_KEY="$(generate_even_key)"
DA3_SIGNER_PRIVATE_KEY="$(generate_even_key)"

export ZEKO_TEST_SEQUENCER_SIGNER="127.0.0.1:8600"
export ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY

MINA_PRIVATE_KEY="$ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY" \
  "$SEQUENCER_SIGNER_BIN" run --port 8600 --allow-zkapp-signing --max-fee 10 --max-balance-change 1000000 &
signer_seq_pid=$!
SIGNER_PIDS+=("$signer_seq_pid")

MINA_PRIVATE_KEY="$DA1_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8601 --allow-field-signing &
signer_da1_pid=$!
SIGNER_PIDS+=("$signer_da1_pid")

MINA_PRIVATE_KEY="$DA2_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8602 --allow-field-signing &
signer_da2_pid=$!
SIGNER_PIDS+=("$signer_da2_pid")

MINA_PRIVATE_KEY="$DA3_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8603 --allow-field-signing &
signer_da3_pid=$!
SIGNER_PIDS+=("$signer_da3_pid")

wait_for_port 8600 $signer_seq_pid
wait_for_port 8601 $signer_da1_pid
wait_for_port 8602 $signer_da2_pid
wait_for_port 8603 $signer_da3_pid

$SEQUENCER_BUILD_ROOT/tests/testing_ledger/run.exe -p 8080 --db-dir "$TMP_DIR/l1_db" --network-id testnet --block-period 9999999 &
l1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8555 --healthcheck-port 8558 --network-id testnet --db-dir "$TMP_DIR/da1_db" --signer 127.0.0.1:8601 &
da1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8556 --healthcheck-port 8559 --network-id testnet --db-dir "$TMP_DIR/da2_db" --signer 127.0.0.1:8602 &
da2_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8557 --healthcheck-port 8560 --network-id testnet --db-dir "$TMP_DIR/da3_db" --signer 127.0.0.1:8603 &
da3_pid=$!

# Launch provers
if [ "$MODE" = "fake" ]; then
  # For some reason prover can't connect immediately to message queue sometimes
  sleep 5
  BIN="$SEQUENCER_BUILD_ROOT/prover/cli_fake.exe"
else
  BIN="$SEQUENCER_BUILD_ROOT/prover/cli.exe"
fi
for ((i = 0; i < NUM_PROVERS; i++)); do
  PORT=$((9990 + i))
  ZEKO_CIRCUITS_MODE=$MODE $BIN run-server --mq-host "localhost:5672" &
  PROVER_PID=$!
  PROVER_PIDS+=("$PROVER_PID")
  PROVERS+=("localhost:$PORT")
done

# Wait for ports to be open
echo "Waiting for services to start..."

wait_for_port 8080 $l1_pid
wait_for_port 8555 $da1_pid
wait_for_port 8556 $da2_pid
wait_for_port 8557 $da3_pid

echo "All services started successfully"

if [ "$MODE" = "fake" ]; then
  ZEKO_CIRCUITS_MODE=$MODE  $SEQUENCER_BUILD_ROOT/tests/sequencer_test_fake.exe "${PROVERS[@]}"
else
  ZEKO_CIRCUITS_MODE=$MODE $SEQUENCER_BUILD_ROOT/tests/sequencer_test.exe "${PROVERS[@]}"
fi
