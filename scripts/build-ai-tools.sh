#!/usr/bin/env bash

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

export NIXPKGS_ALLOW_UNFREE=1

COMMON_NIX_EXPR='
  let
    flake = builtins.getFlake (toString ./.);
    system = builtins.currentSystem;
    pkgs = import flake.inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        (_final: _prev: {
          unstable = import flake.inputs.nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
          };
        })
      ];
    };
  in
'

build_package() {
  local label="$1"
  local relpath="$2"

  echo "Building ${label}..."
  nix build --impure --no-link --print-out-paths --expr \
    "${COMMON_NIX_EXPR} pkgs.callPackage ${relpath} {}"
}

build_tool() {
  case "$1" in
  claude-code)
    build_package "claude-code" ./config/home-manager/home/packages/claude-code.nix
    ;;
  codex-cli)
    build_package "codex-cli" ./config/home-manager/home/packages/codex.nix
    ;;
  droid)
    build_package "droid" ./config/home-manager/home/packages/droid.nix
    ;;
  gemini-cli)
    build_package "gemini-cli" ./config/home-manager/home/packages/gemini-cli.nix
    ;;
  *)
    printf 'Unknown AI tool: %s\n' "$1" >&2
    return 2
    ;;
  esac
}

printf 'Verifying AI tool builds...\n\n'

if [ "$#" -eq 0 ]; then
  set -- claude-code codex-cli droid gemini-cli
fi

for tool in "$@"; do
  build_tool "$tool"
  printf '\n'
done

echo "Requested AI tool builds succeeded."
