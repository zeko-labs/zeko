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
  kill ${l1_pid:-} ${da1_pid:-} ${da2_pid:-} ${da3_pid:-} "${PROVER_PIDS[@]}" 2>/dev/null
  rm -rf "$TMP_DIR"
  docker rm -f pg-sequencer 2>/dev/null
  docker rm -f rabbitmq-sequencer 2>/dev/null
  exit ${exit_status:-0}
}

trap 'cleanup 1' SIGINT SIGTERM
trap 'cleanup $?' EXIT

SEQUENCER_ROOT="$(git rev-parse --show-toplevel)/src/app/zeko/sequencer"
SEQUENCER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/sequencer"

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

$SEQUENCER_BUILD_ROOT/tests/testing_ledger/run.exe -p 8080 --db-dir "$TMP_DIR/l1_db" --network-id testnet --block-period 9999999 &
l1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8555 --random-sk --network-id testnet --db-dir "$TMP_DIR/da1_db" &
da1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8556 --random-sk --network-id testnet --db-dir "$TMP_DIR/da2_db" &
da2_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8557 --random-sk --network-id testnet --db-dir "$TMP_DIR/da3_db" &
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
  $BIN run-server --mq-host "localhost:5672" >/dev/null &
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
  $SEQUENCER_BUILD_ROOT/tests/sequencer_test_fake.exe "${PROVERS[@]}"
else
  $SEQUENCER_BUILD_ROOT/tests/sequencer_test.exe "${PROVERS[@]}"
fi
