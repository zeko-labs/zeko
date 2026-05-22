#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <fake|real> <num_provers> <agent?> <wait_for_port?>" >&2
  echo "   or: $0 <fake|real> <num_provers> explorer-e2e" >&2
  exit 1
}

if [ "$#" -ne 3 ] && [ "$#" -ne 4 ]; then
  usage
fi

MODE="$1"
NUM_PROVERS="$2"
PROVER_PIDS=()
SIGNER_PIDS=()
PROVERS=()

if [ "$#" -eq 3 ]; then
  TEST_TARGET="$3"
  AGENT="false"
  WAIT_FOR_PORT="true"
else
  TEST_TARGET="sequencer"
  AGENT="$3"
  WAIT_FOR_PORT="$4"
fi

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

case "$TEST_TARGET" in
sequencer | explorer-e2e) ;;
*)
  echo "Error: third argument must be 'explorer-e2e' when using 3-arg mode" >&2
  usage
  ;;
esac

if [ "$AGENT" = "true" ]; then
  echo "Redirecting output to /tmp/sequencer_test_output.log"
  exec > /tmp/sequencer_test_output.log 2>&1
fi

cleanup() {
  local exit_status=$1
  echo "Cleaning up..."
  local -a service_pids=(
    "${l1_pid:-}"
    "${da1_pid:-}"
    "${da2_pid:-}"
    "${da3_pid:-}"
    "${PROVER_PIDS[@]}"
    "${SIGNER_PIDS[@]}"
  )

  trap - SIGINT SIGTERM EXIT

  for pid in "${service_pids[@]}"; do
    [ -n "$pid" ] || continue
    kill_process_tree TERM "$pid"
  done

  sleep 1

  for pid in "${service_pids[@]}"; do
    [ -n "$pid" ] || continue
    kill_process_tree KILL "$pid"
  done

  [ -z "${TMP_DIR:-}" ] || rm -rf "$TMP_DIR"
  docker rm -f pg-sequencer 2>/dev/null
  docker rm -f rabbitmq-sequencer 2>/dev/null
  exit ${exit_status:-0}
}

trap 'cleanup 1' SIGINT SIGTERM
trap 'cleanup $?' EXIT

SEQUENCER_ROOT="$(git rev-parse --show-toplevel)/src/app/zeko/sequencer"
SEQUENCER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/sequencer"
SIGNER_BUILD_ROOT="$(git rev-parse --show-toplevel)/_build/default/src/app/zeko/signer"

test_binary() {
  if [ "$TEST_TARGET" = "explorer-e2e" ]; then
    echo "$SEQUENCER_BUILD_ROOT/explorer/tests/explorer_e2e_gherkin_tests.exe"
  elif [ "$MODE" = "fake" ]; then
    echo "$SEQUENCER_BUILD_ROOT/tests/sequencer_test_fake.exe"
  else
    echo "$SEQUENCER_BUILD_ROOT/tests/sequencer_test.exe"
  fi
}

ensure_explorer_e2e_build() {
  if [ "$TEST_TARGET" != "explorer-e2e" ]; then
    return
  fi

  opam exec -- env -u DUNE_RPC dune build \
    src/app/zeko/sequencer/tests/testing_ledger/run.exe \
    src/app/zeko/da_layer/cli.exe \
    src/app/zeko/sequencer/cli.exe \
    src/app/zeko/sequencer/prover/cli.exe \
    src/app/zeko/sequencer/prover/cli_fake.exe \
    src/app/zeko/signer/cli.exe \
    src/app/zeko/sequencer/explorer/tests/explorer_e2e_gherkin_tests.exe
}

ensure_explorer_e2e_build

export ZEKO_SIGNATURE_KIND=zeko-testnet
export ZEKO_CIRCUITS_CONFIG=test

TMP_DIR=$(mktemp -d)

list_child_pids() {
  local parent_pid="$1"
  ps -axo pid=,ppid= | awk -v parent="$parent_pid" '$2 == parent { print $1 }'
}

kill_process_tree() {
  local signal="$1"
  local pid="$2"
  local child_pid

  while read -r child_pid; do
    [ -n "$child_pid" ] || continue
    kill_process_tree "$signal" "$child_pid"
  done < <(list_child_pids "$pid")

  kill "-$signal" "$pid" 2>/dev/null || true
}

wait_for_port() {
  if [ "$WAIT_FOR_PORT" = "true" ]; then
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
  else
    echo "Waiting for 2 seconds..."
    sleep 2
  fi
}

wait_for_queue_consumers() {
  local queue=$1
  local expected_consumers=$2
  local timeout_seconds=${3:-300}

  for _ in $(seq 1 "$timeout_seconds"); do
    if docker exec rabbitmq-sequencer rabbitmqctl -q list_queues name consumers \
      | awk -v queue="$queue" -v expected="$expected_consumers" \
          '$1 == queue && $2 >= expected { found = 1 } END { exit found ? 0 : 1 }'; then
      echo "Queue $queue has at least $expected_consumers consumers"
      return
    fi

    for pid in "${PROVER_PIDS[@]}"; do
      if ! kill -0 "$pid" 2>/dev/null; then
        echo "Prover process $pid exited before registering as a queue consumer"
        exit 1
      fi
    done

    sleep 1
  done

  echo "Timed out waiting for $expected_consumers consumers on queue $queue"
  docker logs rabbitmq-sequencer || true
  exit 1
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
  rabbitmq:3.13.7

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

run() {
  local name="$1"
  shift

  bash -c '
    name="$1"
    shift

    exec stdbuf -oL -eL "$@" \
      > >(awk -v n="$name" '"'"'{ print "[" n "] " $0; fflush(); }'"'"') \
      2> >(awk -v n="$name" '"'"'{ print "[" n "][ERR] " $0; fflush(); }'"'"' >&2)
  ' bash "$name" "$@"
}

run "sequencer-signer" \
  env MINA_PRIVATE_KEY="$ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY" \
  "$SEQUENCER_SIGNER_BIN" run --port 8600 --allow-zkapp-signing --max-fee 10 --max-balance-change 1000000 &
signer_seq_pid=$!
SIGNER_PIDS+=("$signer_seq_pid")

run "da1-signer" \
  env MINA_PRIVATE_KEY="$DA1_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8601 --allow-field-signing &
signer_da1_pid=$!
SIGNER_PIDS+=("$signer_da1_pid")

run "da2-signer" \
  env MINA_PRIVATE_KEY="$DA2_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8602 --allow-field-signing &
signer_da2_pid=$!
SIGNER_PIDS+=("$signer_da2_pid")

run "da3-signer" \
  env MINA_PRIVATE_KEY="$DA3_SIGNER_PRIVATE_KEY" \
  "$DA_SIGNER_BIN" run --port 8603 --allow-field-signing &
signer_da3_pid=$!
SIGNER_PIDS+=("$signer_da3_pid")

wait_for_port 8600 $signer_seq_pid
wait_for_port 8601 $signer_da1_pid
wait_for_port 8602 $signer_da2_pid
wait_for_port 8603 $signer_da3_pid

run "l1" $SEQUENCER_BUILD_ROOT/tests/testing_ledger/run.exe -p 8080 --db-dir "$TMP_DIR/l1_db" --network-id mainnet --block-period 9999999 &
l1_pid=$!

run "da1" $SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8555 --healthcheck-port 8558 --network-id zeko-testnet --db-dir "$TMP_DIR/da1_db" --signer 127.0.0.1:8601 &
da1_pid=$!

run "da2" $SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8556 --healthcheck-port 8559 --network-id zeko-testnet --db-dir "$TMP_DIR/da2_db" --signer 127.0.0.1:8602 &
da2_pid=$!

run "da3" $SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node --port 8557 --healthcheck-port 8560 --network-id zeko-testnet --db-dir "$TMP_DIR/da3_db" --signer 127.0.0.1:8603 &
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
  run "prover-$i" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    $BIN run-server --mq-host "localhost:5672" &
  PROVER_PID=$!
  PROVER_PIDS+=("$PROVER_PID")
  PROVERS+=("localhost:$PORT")
done

echo "Waiting for services to start..."

wait_for_port 8080 $l1_pid
wait_for_port 8555 $da1_pid
wait_for_port 8556 $da2_pid
wait_for_port 8557 $da3_pid
wait_for_queue_consumers sequencer.jobs "$NUM_PROVERS"

echo "All services started successfully"

if [ "$TEST_TARGET" = "explorer-e2e" ]; then
  run "explorer-e2e" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    "$(test_binary)"
elif [ "$MODE" = "fake" ]; then
  run "sequencer-test" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    "$(test_binary)" "${PROVERS[@]}"
else
  run "sequencer-test" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    "$(test_binary)" "${PROVERS[@]}"
fi
