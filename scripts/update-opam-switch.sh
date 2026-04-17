#!/usr/bin/env bash

# Reuses a shared opam switch cache keyed by the exported toolchain. Each
# checkout keeps a local `_opam` symlink pointing at the shared cache entry so
# identical dependency snapshots can be reused across worktrees. The opam
# package universe is pinned to the same ocaml/opam-repository commit used in CI
# so local resolution matches CI.

set -eo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR/.."

ensure_homebrew_deps() {
    local missing=()
    local formula

    for formula in \
        boost \
        capnp \
        gflags \
        gmp \
        jemalloc \
        libffi \
        libsodium \
        lmdb \
        pkg-config \
        rocksdb \
        zlib
    do
        brew list --formula "${formula}" >/dev/null 2>&1 || missing+=("${formula}")
    done

    if ! brew list --formula openssl@1.1.1 >/dev/null 2>&1 \
        && ! brew list --formula openssl@3 >/dev/null 2>&1; then
        missing+=("openssl@1.1.1")
    fi

    if ! brew list --formula postgresql@14 >/dev/null 2>&1 \
        && ! brew list --formula postgresql@18 >/dev/null 2>&1 \
        && ! brew list --formula postgresql >/dev/null 2>&1; then
        missing+=("postgresql@18")
    fi

    if ((${#missing[@]} > 0)); then
        brew install "${missing[@]}"
    fi
}

prepend_path_var() {
    local var_name="$1"
    local dir="$2"
    local current="${!var_name:-}"

    [[ -d "${dir}" ]] || return 0

    if [[ -z "${current}" ]]; then
        printf -v "${var_name}" '%s' "${dir}"
    elif [[ ":${current}:" != *":${dir}:"* ]]; then
        printf -v "${var_name}" '%s:%s' "${dir}" "${current}"
    fi

    export "${var_name}"
}

configure_homebrew_env() {
    local prefix
    local openssl_prefix

    if brew list --formula openssl@1.1.1 >/dev/null 2>&1; then
        openssl_prefix="$(brew --prefix openssl@1.1.1)"
    else
        openssl_prefix="$(brew --prefix openssl@3)"
    fi

    for prefix in \
        "$(brew --prefix gmp)" \
        "$(brew --prefix libffi)" \
        "$(brew --prefix lmdb)" \
        "${openssl_prefix}" \
        "$(brew --prefix zlib)"
    do
        prepend_path_var CPATH "${prefix}/include"
        prepend_path_var LIBRARY_PATH "${prefix}/lib"
        prepend_path_var PKG_CONFIG_PATH "${prefix}/lib/pkgconfig"
    done

    prepend_path_var PATH "$(brew --prefix lmdb)/bin"

    export CFLAGS="${CFLAGS:+${CFLAGS} }-I$(brew --prefix gmp)/include"
    export CPPFLAGS="${CPPFLAGS:+${CPPFLAGS} }-I$(brew --prefix gmp)/include"
    export LDFLAGS="${LDFLAGS:+${LDFLAGS} }-L$(brew --prefix gmp)/lib"

    if [[ "${openssl_prefix}" == "$(brew --prefix openssl@1.1.1)" ]]; then
        export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration -Wno-incompatible-function-pointer-types -Wno-deprecated-declarations"
    fi
}

# Don't do anything if we're in a nix shell
[[ "$IN_NIX_SHELL$CI$BUILDKITE" == "" ]] || exit 0

opam_repo_commit="ba1ca7509cb2617776f017673de0f2a48be67105"
repo_cache_root="${XDG_CACHE_HOME:-$HOME/.cache}/zeko"
opam_repo_dir="${repo_cache_root}/opam-repository/${opam_repo_commit}"

sum="$({
    printf '%s\n' "${opam_repo_commit}"
    cat opam.export
} | cksum | awk '{print $1}')"
cache_root="${repo_cache_root}/opam-switches"
switch_dir="${cache_root}/${sum}"

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
    if [[ "$(uname -s)" == "Darwin" ]] && command -v brew >/dev/null 2>&1; then
        ensure_homebrew_deps
        configure_homebrew_env
    fi

    mkdir -p "$(dirname "${opam_repo_dir}")"
    if [[ ! -d "${opam_repo_dir}/.git" ]]; then
        git clone https://github.com/ocaml/opam-repository.git --depth 1 \
            "${opam_repo_dir}"
    fi
    git -C "${opam_repo_dir}" fetch origin "${opam_repo_commit}" --depth 1
    git -C "${opam_repo_dir}" checkout --detach "${opam_repo_commit}"

    if opam repository list --all --short | grep -qx default; then
        opam repository set-url --kind=local default "${opam_repo_dir}"
        opam repository add --yes --all --set-default default
    else
        opam repository add --yes --all --set-default --kind=local default \
            "${opam_repo_dir}"
    fi

    # We add o1-labs opam repository and make it default selection
    # (if it's repeated, it's a no-op).
    opam repository add --yes --all --set-default o1-labs \
         https://github.com/o1-labs/opam-repository.git
    opam update default o1-labs
    opam switch import -y --assume-depexts --switch . opam.export
    mkdir -p "${cache_root}"
    mv _opam "${switch_dir}"
fi

ln -s "${switch_dir}" _opam
