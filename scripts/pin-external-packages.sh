#!/bin/sh

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
PINS_FILE="$SCRIPT_DIR/external-opam-pins.txt"

cd "$REPO_ROOT"

# keep the repo-managed submodules in sync first
git submodule sync && git submodule update --init --recursive

# then install externally pinned opam packages that are not tracked in opam.export
while IFS=' ' read -r package source; do
    case "$package" in
        ''|\#*)
            continue
            ;;
    esac

    opam pin add --yes --no-action "$package" "$source"
done < "$PINS_FILE"

opam install --yes nats-client nats-client-async
