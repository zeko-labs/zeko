#!/bin/bash

cleanup() {
    local exit_status=$1
    echo "Cleaning up..."
    kill $l1_pid $da1_pid $da2_pid $da3_pid $prover1_pid $prover2_pid 2>/dev/null
    rm -rf "$TMP_DIR"
    docker rm -f pg-sequencer 2>/dev/null
    exit ${exit_status:-0}
}

trap 'cleanup 1' SIGINT SIGTERM
trap 'cleanup $?' EXIT

if [ "$1" = "fake" ] || [ "$1" = "real" ]; then
    echo "Mode: $1"
    MODE=$1
else
    echo "Error: First argument must be either 'fake' or 'real'"
    exit 1
fi

SEQUENCER_ROOT="$(git rev-parse --show-toplevel)/src/app/zeko/sequencer"
SEQUENCER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/sequencer"

export ZEKO_SIGNATURE_KIND=testnet
export ZEKO_CIRCUITS_CONFIG=test

TMP_DIR=$(mktemp -d)

docker run --rm --name pg-sequencer \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid \
  -p 5433:5432 \
  -d postgres:16-alpine

$SEQUENCER_BUILD_ROOT/tests/testing_ledger/run.exe -p 8080 --db-dir "$TMP_DIR/l1_db" --network-id testnet --block-period 9999999 &
l1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8555 --random-sk --network-id testnet --db-dir "$TMP_DIR/da1_db" &
da1_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8556 --random-sk --network-id testnet --db-dir "$TMP_DIR/da2_db" &
da2_pid=$!

$SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8557 --random-sk --network-id testnet --db-dir "$TMP_DIR/da3_db" &
da3_pid=$!

if [ "$MODE" = "fake" ]; then
    $SEQUENCER_BUILD_ROOT/prover/cli_fake.exe run-server --port 9990 &
    prover1_pid=$!

    $SEQUENCER_BUILD_ROOT/prover/cli_fake.exe run-server --port 9991 &
    prover2_pid=$!
else
    $SEQUENCER_BUILD_ROOT/prover/cli.exe run-server --port 9990 &
    prover1_pid=$!

    $SEQUENCER_BUILD_ROOT/prover/cli.exe run-server --port 9991 &
    prover2_pid=$!
fi

# Wait for ports to be open
echo "Waiting for services to start..."

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

wait_for_port 8080 $l1_pid
wait_for_port 8555 $da1_pid
wait_for_port 8556 $da2_pid
wait_for_port 8557 $da3_pid
wait_for_port 9990 $prover1_pid
wait_for_port 9991 $prover2_pid

echo "All services started successfully"

if [ "$MODE" = "fake" ]; then
    $SEQUENCER_BUILD_ROOT/tests/sequencer_test_fake.exe
else
    $SEQUENCER_BUILD_ROOT/tests/sequencer_test.exe
fi
