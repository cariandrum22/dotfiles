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

printf 'Verifying AI tool builds...\n\n'

build_package "claude-code" ./config/home-manager/home/packages/claude-code.nix
printf '\n'
build_package "codex-cli" ./config/home-manager/home/packages/codex.nix
printf '\n'
build_package "droid" ./config/home-manager/home/packages/droid.nix
printf '\n'
build_package "gemini-cli" ./config/home-manager/home/packages/gemini-cli.nix
printf '\n'

echo "All AI tool builds succeeded."
