#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <output-directory> <da-node-count> <da-quorum>" >&2
  exit 2
}

[[ $# -eq 3 ]] || usage
OUTPUT_DIRECTORY=$1
DA_NODE_COUNT=$2
DA_QUORUM=$3
[[ $DA_NODE_COUNT =~ ^[1-3]$ ]] || {
  echo "DA node count must be between 1 and 3" >&2
  exit 1
}
[[ $DA_QUORUM =~ ^[1-3]$ && $DA_QUORUM -le $DA_NODE_COUNT ]] || {
  echo "DA quorum must be between 1 and the DA node count" >&2
  exit 1
}
[[ -d $OUTPUT_DIRECTORY ]] || {
  echo "Output directory does not exist: $OUTPUT_DIRECTORY" >&2
  exit 1
}
OUTPUT_DIRECTORY=$(realpath "$OUTPUT_DIRECTORY")

for command in awk docker dune nc openssl pgrep realpath; do
  command -v "$command" >/dev/null || {
    echo "Missing command: $command" >&2
    exit 1
  }
done

for variable in ZEKO_CIRCUITS_CONFIG ZEKO_DEPLOY_CONFIG \
  ZEKO_DEPLOYMENT_SEQUENCER_PRIVATE_KEY; do
  [[ -n ${!variable:-} ]] || {
    echo "$variable is required" >&2
    exit 1
  }
done
for ((index = 1; index <= DA_NODE_COUNT; index++)); do
  variable="ZEKO_DEPLOYMENT_DA${index}_PRIVATE_KEY"
  [[ -n ${!variable:-} ]] || {
    echo "$variable is required" >&2
    exit 1
  }
done

ROOT=$(git rev-parse --show-toplevel)
BUILD_ROOT=$ROOT/_build/default/src/app/zeko
TMP_DIRECTORY=$(mktemp -d)
POSTGRES_CONTAINER="zeko-deployment-export-postgres-$$"
RABBITMQ_CONTAINER="zeko-deployment-export-rabbitmq-$$"
SERVICE_PIDS=()
SIGNING_NETWORK_ID=${MINA_SIGNING_NETWORK_ID:-testnet}
export ZEKO_SIGNATURE_KIND=$SIGNING_NETWORK_ID
export ZEKO_TEST_L1_NETWORK_ID=$SIGNING_NETWORK_ID
export ZEKO_SIGNER_AUTH_TOKEN=deployment-export-signer-token
export ZEKO_SIGNER_TLS_HOSTNAME=localhost

terminate_tree() {
  local pid=$1 child
  while read -r child; do
    [[ -n $child ]] && terminate_tree "$child"
  done < <(pgrep -P "$pid" 2>/dev/null || true)
  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
}

cleanup() {
  local status=$?
  trap - EXIT INT TERM
  for pid in "${SERVICE_PIDS[@]}"; do
    terminate_tree "$pid"
  done
  docker rm -f "$POSTGRES_CONTAINER" "$RABBITMQ_CONTAINER" \
    >/dev/null 2>&1 || true
  rm -rf "$TMP_DIRECTORY"
  exit "$status"
}
trap cleanup EXIT INT TERM

wait_for_port() {
  local port=$1 pid=$2
  for _ in $(seq 1 600); do
    nc -z 127.0.0.1 "$port" 2>/dev/null && return 0
    kill -0 "$pid" 2>/dev/null || {
      echo "Process for port $port exited" >&2
      return 1
    }
    sleep 1
  done
  echo "Timed out waiting for port $port" >&2
  return 1
}

port_is_listening() {
  local port_hex
  printf -v port_hex '%04X' "$1"
  awk -v port=":$port_hex" \
    '$2 ~ (port "$") && $4 == "0A" { found = 1 } END { exit !found }' \
    /proc/net/tcp /proc/net/tcp6 2>/dev/null
}

wait_for_listen() {
  local port=$1 pid=$2
  for _ in $(seq 1 600); do
    port_is_listening "$port" && return 0
    kill -0 "$pid" 2>/dev/null || {
      echo "Process for port $port exited" >&2
      return 1
    }
    sleep 1
  done
  echo "Timed out waiting for port $port" >&2
  return 1
}

wait_for_container_port() {
  local port=$1 container=$2
  for _ in $(seq 1 600); do
    nc -z 127.0.0.1 "$port" 2>/dev/null && return 0
    [[ $(docker inspect -f '{{.State.Running}}' "$container" 2>/dev/null) == true ]] || {
      echo "Container for port $port exited" >&2
      return 1
    }
    sleep 1
  done
  echo "Timed out waiting for port $port" >&2
  return 1
}

wait_for_prover() {
  local pid=$1 consumers
  for _ in $(seq 1 1800); do
    consumers=$(
      { docker exec "$RABBITMQ_CONTAINER" rabbitmqctl -q list_queues \
        name consumers 2>/dev/null || true; } \
        | awk '$2 ~ /^[0-9]+$/ { total += $2 } END { print total + 0 }'
    )
    [[ $consumers -ge 1 ]] && return 0
    kill -0 "$pid" 2>/dev/null || {
      echo "Prover exited before accepting jobs" >&2
      return 1
    }
    sleep 1
  done
  echo "Timed out waiting for prover circuit compilation" >&2
  return 1
}

requested_offset=${ZEKO_DEPLOYMENT_EXPORT_PORT_OFFSET:-}
if [[ -n $requested_offset && ! $requested_offset =~ ^[0-9]+$ ]]; then
  echo "ZEKO_DEPLOYMENT_EXPORT_PORT_OFFSET must be a non-negative integer" >&2
  exit 1
fi
if [[ -n $requested_offset ]]; then
  port_offsets=("$requested_offset")
else
  port_offsets=(0 10000 20000 30000 40000)
fi
PORT_OFFSET=
for candidate in "${port_offsets[@]}"; do
  ((candidate <= 50000)) || continue
  candidate_ports=("$((5433 + candidate))" "$((5672 + candidate))" \
    "$((8080 + candidate))" "$((8600 + candidate))")
  for ((index = 1; index <= DA_NODE_COUNT; index++)); do
    candidate_ports+=("$((8554 + candidate + index))" \
      "$((8557 + candidate + index))" "$((8600 + candidate + index))")
  done
  ports_available=true
  for port in "${candidate_ports[@]}"; do
    port_is_listening "$port" && ports_available=false
  done
  if [[ $ports_available == true ]]; then
    PORT_OFFSET=$candidate
    break
  fi
done
[[ -n $PORT_OFFSET ]] || {
  echo "Could not find an available local port range" >&2
  exit 1
}
POSTGRES_PORT=$((5433 + PORT_OFFSET))
RABBITMQ_PORT=$((5672 + PORT_OFFSET))
L1_PORT=$((8080 + PORT_OFFSET))
SEQUENCER_SIGNER_PORT=$((8600 + PORT_OFFSET))
if ((PORT_OFFSET > 0)); then
  echo "Using local service port offset $PORT_OFFSET" >&2
fi

dune build \
  ./src/app/zeko/sequencer/deployment_export.exe \
  ./src/app/zeko/sequencer/prover/cli.exe \
  ./src/app/zeko/sequencer/tests/testing_ledger/run.exe \
  ./src/app/zeko/da_layer/cli.exe \
  ./src/app/zeko/signer/cli.exe

docker run --rm --name "$POSTGRES_CONTAINER" \
  -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid \
  -p "127.0.0.1:$POSTGRES_PORT:5432" \
  -d "${POSTGRES_IMAGE:-postgres:16-alpine}" \
  >/dev/null
docker run --rm --name "$RABBITMQ_CONTAINER" \
  -p "127.0.0.1:$RABBITMQ_PORT:5672" \
  -d "${RABBITMQ_IMAGE:-rabbitmq:4-alpine}" \
  >/dev/null
wait_for_container_port "$POSTGRES_PORT" "$POSTGRES_CONTAINER"
wait_for_container_port "$RABBITMQ_PORT" "$RABBITMQ_CONTAINER"

openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
  -keyout "$TMP_DIRECTORY/signer.key" -out "$TMP_DIRECTORY/signer.crt" \
  -subj /CN=localhost -addext subjectAltName=DNS:localhost \
  >/dev/null 2>&1
export ZEKO_SIGNER_TLS_CA_FILE=$TMP_DIRECTORY/signer.crt

env MINA_PRIVATE_KEY="$ZEKO_DEPLOYMENT_SEQUENCER_PRIVATE_KEY" \
  "$BUILD_ROOT/signer/cli.exe" run --port "$SEQUENCER_SIGNER_PORT" \
    --allow-zkapp-signing \
    --max-fee 10 --max-balance-change 1000000 \
    --tls-cert-file "$TMP_DIRECTORY/signer.crt" \
    --tls-key-file "$TMP_DIRECTORY/signer.key" &
service_pid=$!
SERVICE_PIDS+=("$service_pid")
wait_for_listen "$SEQUENCER_SIGNER_PORT" "$service_pid"

DA_NODES=()
for ((index = 1; index <= DA_NODE_COUNT; index++)); do
  signer_port=$((8600 + PORT_OFFSET + index))
  da_port=$((8554 + PORT_OFFSET + index))
  health_port=$((8557 + PORT_OFFSET + index))
  variable="ZEKO_DEPLOYMENT_DA${index}_PRIVATE_KEY"
  env MINA_PRIVATE_KEY="${!variable}" \
    "$BUILD_ROOT/signer/cli.exe" run --port "$signer_port" \
      --allow-field-signing --tls-cert-file "$TMP_DIRECTORY/signer.crt" \
      --tls-key-file "$TMP_DIRECTORY/signer.key" &
  service_pid=$!
  SERVICE_PIDS+=("$service_pid")
  wait_for_listen "$signer_port" "$service_pid"
  "$BUILD_ROOT/da_layer/cli.exe" run-node --bind-localhost --port "$da_port" \
    --healthcheck-port "$health_port" --network-id "$SIGNING_NETWORK_ID" \
    --db-dir "$TMP_DIRECTORY/da${index}" \
    --signer "localhost:$signer_port" &
  service_pid=$!
  SERVICE_PIDS+=("$service_pid")
  wait_for_port "$da_port" "$service_pid"
  DA_NODES+=("127.0.0.1:$da_port")
done

"$BUILD_ROOT/sequencer/tests/testing_ledger/run.exe" -p "$L1_PORT" \
  --db-dir "$TMP_DIRECTORY/l1" --network-id "$SIGNING_NETWORK_ID" \
  --block-period 9999999 &
service_pid=$!
SERVICE_PIDS+=("$service_pid")
wait_for_port "$L1_PORT" "$service_pid"

"$BUILD_ROOT/sequencer/prover/cli.exe" run-server \
  --mq-host "127.0.0.1:$RABBITMQ_PORT" &
service_pid=$!
SERVICE_PIDS+=("$service_pid")
wait_for_prover "$service_pid"

DA_NODE_ARGUMENTS=()
for node in "${DA_NODES[@]}"; do
  DA_NODE_ARGUMENTS+=(--da-node "$node")
done
"$BUILD_ROOT/sequencer/deployment_export.exe" \
  --output-directory "$OUTPUT_DIRECTORY" \
  --db-dir "$TMP_DIRECTORY/sequencer" \
  --l1-uri "http://127.0.0.1:$L1_PORT/graphql" \
  --postgres-uri \
    "postgres://postgres:postgres@127.0.0.1:$POSTGRES_PORT/postgres" \
  "${DA_NODE_ARGUMENTS[@]}" --da-quorum "$DA_QUORUM" \
  --mq-host "127.0.0.1:$RABBITMQ_PORT" \
  --signer "127.0.0.1:$SEQUENCER_SIGNER_PORT" \
  --commit-validity-period "${ZEKO_ETHEREUM_COMMIT_VALIDITY_PERIOD:-20}"
