#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <fake|real> <num_provers> <agent?> <wait_for_port?>" >&2
  exit 1
}

if [ "$#" -ne 4 ]; then
  usage
fi

MODE="$1"
NUM_PROVERS="$2"
PROVER_PIDS=()
SIGNER_PIDS=()
DA_PIDS=()
PROVERS=()

AGENT="$3"
if [ "$AGENT" = "true" ]; then
  echo "Redirecting output to /tmp/sequencer_test_output.log"
  exec > /tmp/sequencer_test_output.log 2>&1
fi

WAIT_FOR_PORT="$4"

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

EXPORT_ONLY=false
if [[ ${ZEKO_ETHEREUM_BRIDGE_EXPORT_ONLY:-false} == true || \
      ${ZEKO_ETHEREUM_SEQUENTIAL_EXPORT_ONLY:-false} == true ]]; then
  EXPORT_ONLY=true
fi
if [[ $EXPORT_ONLY == true ]]; then
  ZEKO_TEST_DA_NODE_COUNT=${ZEKO_TEST_DA_NODE_COUNT:-3}
  ZEKO_TEST_DA_QUORUM=${ZEKO_TEST_DA_QUORUM:-2}
else
  ZEKO_TEST_DA_NODE_COUNT=3
  ZEKO_TEST_DA_QUORUM=2
fi
[[ $ZEKO_TEST_DA_NODE_COUNT =~ ^[1-3]$ ]] || {
  echo "ZEKO_TEST_DA_NODE_COUNT must be between 1 and 3" >&2
  exit 1
}
[[ $ZEKO_TEST_DA_QUORUM =~ ^[1-3]$ && \
   $ZEKO_TEST_DA_QUORUM -le $ZEKO_TEST_DA_NODE_COUNT ]] || {
  echo "ZEKO_TEST_DA_QUORUM must be between 1 and ZEKO_TEST_DA_NODE_COUNT" >&2
  exit 1
}
export ZEKO_TEST_DA_NODE_COUNT ZEKO_TEST_DA_QUORUM

cleanup() {
  local exit_status=$1
  echo "Cleaning up..."
  local -a service_pids=(
    "${l1_pid:-}"
    "${DA_PIDS[@]}"
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

# Keep the external signer and DA receipt validation on the same salt as the
# circuit configuration. The retained Ethereum PoC uses Mina's built-in
# `testnet` salt because Auro cannot currently sign a custom network ID;
# existing test callers retain the historic `zeko-testnet` default.
SIGNING_NETWORK_ID="${MINA_SIGNING_NETWORK_ID:-zeko-testnet}"
export ZEKO_SIGNATURE_KIND="$SIGNING_NETWORK_ID"
export ZEKO_CIRCUITS_CONFIG="${ZEKO_CIRCUITS_CONFIG:-test}"
ZEKO_TEST_L1_NETWORK_ID="${ZEKO_TEST_L1_NETWORK_ID:-mainnet}"

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

wait_for_provers() {
  local expected=$1
  local consumers=0

  echo "Waiting for $expected prover(s) to finish compiling circuits..."
  for _ in $(seq 1 600); do
    consumers=$(
      { docker exec rabbitmq-sequencer rabbitmqctl -q list_queues name consumers \
        2>/dev/null || true; } \
        | awk '$2 ~ /^[0-9]+$/ { total += $2 } END { print total + 0 }'
    )
    if [ "$consumers" -ge "$expected" ]; then
      echo "$consumers prover consumer(s) are ready"
      return 0
    fi
    for pid in "${PROVER_PIDS[@]}"; do
      if ! kill -0 "$pid" 2>/dev/null; then
        echo "A prover exited before becoming ready" >&2
        return 1
      fi
    done
    sleep 1
  done
  echo "Timed out waiting for prover consumers" >&2
  return 1
}

KEYGEN_BIN="$SEQUENCER_BUILD_ROOT/cli.exe"

generate_even_key() {
  "$KEYGEN_BIN" generate-even-key | awk -F': ' '/Private key:/ {print $2}'
}

docker run --rm --name pg-sequencer \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid \
  -p 127.0.0.1:5433:5432 \
  -d postgres:16-alpine

docker run -d --name rabbitmq-sequencer \
  -p 127.0.0.1:5672:5672 \
  rabbitmq:latest

wait_for_port 5433 $$
wait_for_port 5672 $$

SEQUENCER_SIGNER_BIN="$SIGNER_BUILD_ROOT/cli.exe"
DA_SIGNER_BIN="$SIGNER_BUILD_ROOT/cli.exe"

ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY="${ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY:-$(generate_even_key)}"
DA_SIGNER_PRIVATE_KEYS=()
for ((index = 1; index <= ZEKO_TEST_DA_NODE_COUNT; index++)); do
  private_key_variable="DA${index}_SIGNER_PRIVATE_KEY"
  private_key=${!private_key_variable:-}
  if [[ -z $private_key ]]; then
    private_key=$(generate_even_key)
  fi
  DA_SIGNER_PRIVATE_KEYS+=("$private_key")
done
ZEKO_SIGNER_AUTH_TOKEN="sequencer-test-signer-token"
SIGNER_TLS_CERT="$TMP_DIR/signer-tls-cert.pem"
SIGNER_TLS_KEY="$TMP_DIR/signer-tls-key.pem"

openssl req -x509 -newkey rsa:2048 -nodes \
  -keyout "$SIGNER_TLS_KEY" \
  -out "$SIGNER_TLS_CERT" \
  -days 1 \
  -subj "/CN=localhost" \
  -addext "subjectAltName=DNS:localhost"

export ZEKO_TEST_SEQUENCER_SIGNER="localhost:8600"
export ZEKO_TEST_SEQUENCER_SIGNER_PRIVATE_KEY
export ZEKO_SIGNER_AUTH_TOKEN
export ZEKO_SIGNER_TLS_CA_FILE="$SIGNER_TLS_CERT"
export ZEKO_SIGNER_TLS_HOSTNAME="localhost"

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
  "$SEQUENCER_SIGNER_BIN" run --port 8600 --allow-zkapp-signing --max-fee 10 --max-balance-change 1000000 \
    --tls-cert-file "$SIGNER_TLS_CERT" --tls-key-file "$SIGNER_TLS_KEY" &
signer_seq_pid=$!
SIGNER_PIDS+=("$signer_seq_pid")

DA_SIGNER_PIDS=()
for ((index = 1; index <= ZEKO_TEST_DA_NODE_COUNT; index++)); do
  signer_port=$((8600 + index))
  run "da${index}-signer" \
    env MINA_PRIVATE_KEY="${DA_SIGNER_PRIVATE_KEYS[index - 1]}" \
    "$DA_SIGNER_BIN" run --port "$signer_port" --allow-field-signing \
      --tls-cert-file "$SIGNER_TLS_CERT" --tls-key-file "$SIGNER_TLS_KEY" &
  signer_pid=$!
  SIGNER_PIDS+=("$signer_pid")
  DA_SIGNER_PIDS+=("$signer_pid")
done

wait_for_port 8600 $signer_seq_pid
for ((index = 1; index <= ZEKO_TEST_DA_NODE_COUNT; index++)); do
  wait_for_port "$((8600 + index))" "${DA_SIGNER_PIDS[index - 1]}"
done

run "l1" $SEQUENCER_BUILD_ROOT/tests/testing_ledger/run.exe -p 8080 --db-dir "$TMP_DIR/l1_db" --network-id "$ZEKO_TEST_L1_NETWORK_ID" --block-period 9999999 &
l1_pid=$!

for ((index = 1; index <= ZEKO_TEST_DA_NODE_COUNT; index++)); do
  da_port=$((8554 + index))
  healthcheck_port=$((8557 + index))
  signer_port=$((8600 + index))
  run "da${index}" $SEQUENCER_BUILD_ROOT/../da_layer/cli.exe run-node \
    --bind-localhost --port "$da_port" --healthcheck-port "$healthcheck_port" \
    --network-id "$SIGNING_NETWORK_ID" --db-dir "$TMP_DIR/da${index}_db" \
    --signer "localhost:$signer_port" &
  DA_PIDS+=("$!")
done

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

# Wait for ports to be open
echo "Waiting for services to start..."

wait_for_port 8080 $l1_pid
for ((index = 1; index <= ZEKO_TEST_DA_NODE_COUNT; index++)); do
  wait_for_port "$((8554 + index))" "${DA_PIDS[index - 1]}"
done
wait_for_provers "$NUM_PROVERS"

echo "All services started successfully"

if [ "$MODE" = "fake" ]; then
  run "sequencer-test" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    $SEQUENCER_BUILD_ROOT/tests/sequencer_test_fake.exe "${PROVERS[@]}"
else
  run "sequencer-test" \
    env ZEKO_CIRCUITS_MODE=$MODE \
    $SEQUENCER_BUILD_ROOT/tests/sequencer_test.exe "${PROVERS[@]}"
fi
