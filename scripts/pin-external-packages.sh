#!/bin/sh

# Installs temporary GitHub-backed opam pins that the repo needs until the
# corresponding packages are published to opam.

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
PINS_FILE="$SCRIPT_DIR/external-opam-pins.txt"

cd "$REPO_ROOT"

# keep the repo-managed submodules in sync first
git submodule sync && git submodule update --init --recursive

# Temporary workaround until nats-client and nats-client-async are published to
# opam. After they are published, remove this pin flow and install the released
# packages via opam.export / normal opam dependency resolution instead.
while IFS=' ' read -r package source; do
    case "$package" in
        ''|\#*)
            continue
            ;;
    esac

    opam pin add --yes --no-action "$package" "$source"
done < "$PINS_FILE"

opam install --yes nats-client nats-client-async
