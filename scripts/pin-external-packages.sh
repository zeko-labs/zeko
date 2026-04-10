#!/bin/sh

# Runs the existing external package bootstrap flow for this repo. For now,
# that also includes temporary GitHub-backed opam pins for nats-client and
# nats-client-async until those packages are published to opam.

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
PINS_FILE="$SCRIPT_DIR/external-opam-pins.txt"

cd "$REPO_ROOT"

# keep the repo-managed submodules in sync first
git submodule sync && git submodule update --init --recursive

# Temporary workaround until nats-client and nats-client-async are published to
# opam. After they are published, keep this script but remove these temporary
# pin entries and let normal opam dependency resolution install the releases.
while IFS=' ' read -r package source; do
    case "$package" in
        ''|\#*)
            continue
            ;;
    esac

    opam pin add --switch . --yes --no-action "$package" "$source"
done < "$PINS_FILE"

opam install --switch . --yes nats-client nats-client-async
