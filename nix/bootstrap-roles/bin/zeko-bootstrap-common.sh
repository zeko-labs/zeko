#!/usr/bin/env bash
# Shared helpers for the externally orchestrated bootstrap containers.
# Sourced by the role entrypoints; never run directly.
set -euo pipefail

bootstrap_fail() {
  echo "zeko-bootstrap: $*" >&2
  exit 1
}

# Read one secret file into a variable without echoing it. Secrets are
# Compose-mounted files under /run/secrets; they are never logged.
bootstrap_read_secret() {
  local variable=$1 file=$2
  [[ -f $file ]] || bootstrap_fail "missing secret file $file"
  printf -v "$variable" '%s' "$(<"$file")"
  [[ -n ${!variable} ]] || bootstrap_fail "secret file $file is empty"
  export "$variable"
}

# Circuit and deploy configuration are mandatory. Zeko's
# zeko_circuits_config.ml selects embedded test keys when
# ZEKO_CIRCUITS_CONFIG is unset or "test"; this bootstrap refuses that path.
bootstrap_require_circuit_config() {
  [[ -n ${ZEKO_CIRCUITS_CONFIG:-} && ${ZEKO_CIRCUITS_CONFIG} != test ]] \
    || bootstrap_fail "ZEKO_CIRCUITS_CONFIG must name an operator circuits-config file"
  [[ -f $ZEKO_CIRCUITS_CONFIG ]] \
    || bootstrap_fail "circuits config not mounted at $ZEKO_CIRCUITS_CONFIG"
  [[ -n ${ZEKO_DEPLOY_CONFIG:-} && -f $ZEKO_DEPLOY_CONFIG ]] \
    || bootstrap_fail "ZEKO_DEPLOY_CONFIG must name a mounted deploy-config file"
}

bootstrap_require_signer_tls() {
  [[ -f ${ZEKO_SIGNER_TLS_CA_FILE:-} ]] \
    || bootstrap_fail "signer TLS certificate not found at ${ZEKO_SIGNER_TLS_CA_FILE:-unset}; run the signer-tls service first"
}

# Zeko's message_queue.ml reads RABBITMQ_USER/RABBITMQ_PASSWORD for the AMQP
# login (both the prover consumer and the exporter producer). The broker's
# built-in guest user is limited to loopback connections, so a dedicated user
# is mandatory for container-to-container traffic.
bootstrap_require_mq_credentials() {
  [[ -n ${RABBITMQ_USER:-} && -n ${RABBITMQ_PASSWORD:-} ]] \
    || bootstrap_fail "RABBITMQ_USER and RABBITMQ_PASSWORD are required"
  [[ $RABBITMQ_USER != guest ]] \
    || bootstrap_fail "RABBITMQ_USER must not be guest (loopback-only broker user)"
}

# The host workspace holding secrets, config, state and export output must
# live outside every source checkout: `nix build path:.` copies the whole
# checkout into the world-readable Nix store before any filtering. The
# container cannot see the host tree, so this checks the declared host path
# against the repository layouts; it is a guard, not proof of a safe location.
bootstrap_require_external_workspace() {
  local workspace=${ZEKO_BOOTSTRAP_WORKSPACE_HOST:-}
  [[ $workspace == /* ]] \
    || bootstrap_fail "ZEKO_BOOTSTRAP_WORKSPACE must be an absolute host path outside all source checkouts"
  case $workspace in
    */nix/bootstrap | */nix/bootstrap/* | */nix/bootstrap-roles | */nix/bootstrap-roles/* | */nix/rollup-artifacts | */nix/rollup-artifacts/* | */ethereum-settlement | */ethereum-settlement/* | */src/app/zeko | */src/app/zeko/*)
      bootstrap_fail "ZEKO_BOOTSTRAP_WORKSPACE $workspace lies inside a source checkout layout; use a persistent directory outside all checkouts"
      ;;
  esac
}

bootstrap_wait_tcp() {
  local host=$1 port=$2 attempts=${3:-600}
  for ((i = 0; i < attempts; i++)); do
    if (exec 3<>"/dev/tcp/$host/$port") 2>/dev/null; then
      exec 3>&- 2>/dev/null || true
      return 0
    fi
    sleep 1
  done
  bootstrap_fail "timed out waiting for $host:$port"
}
