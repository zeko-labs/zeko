#!/usr/bin/env bash

# Reuses a cached repo-local opam switch keyed by the exported toolchain plus
# any temporary external pin manifest entries.

set -eo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR/.."

# Don't do anything if we're in a nix shell
[[ "$IN_NIX_SHELL$CI$BUILDKITE" == "" ]] || exit 0

sum="$(
    # Temporary workaround until nats-client and nats-client-async are
    # published to opam. The switch/cache key must include the pin manifest
    # while the repo still depends on GitHub pins.
    cat opam.export scripts/external-opam-pins.txt \
    | cksum \
    | grep -oE '^\S*'
)"
switch_dir=opam_switches/"$sum"

if [[ -d _opam ]]; then
    read -rp "Directory '_opam' exists and will be removed. Continue? [y/N] " \
         confirm
    if [[ "${confirm}" =~ ^[Yy]$ ]]; then
        rm -Rf _opam
    else
        echo "Aborted."
        exit 1
    fi
fi

if [[ ! -d "${switch_dir}" ]]; then
    # We add o1-labs opam repository and make it default
    # (if it's repeated, it's a no-op)
    opam repository add --yes --all --set-default o1-labs \
         https://github.com/o1-labs/opam-repository.git
    opam update
    opam switch import -y --switch . opam.export
    mkdir -p opam_switches
    mv _opam "${switch_dir}"
fi

ln -s "${switch_dir}" _opam

# This existing bootstrap step also applies temporary GitHub pins for
# nats-client and nats-client-async until those packages are published to opam.
./scripts/pin-external-packages.sh
