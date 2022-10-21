#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

#######################################
# Install Home Manager
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   nixpkgs_version::String
# Returns:
#   None
#######################################
install::home_manager() {
  if [[ ("${OSTYPE}" != "darwin"* && "${OSTYPE}" != "linux-gnu"*) ]]; then
    error "This Platform is not supported."
  fi

  local -ra argv=("${@}")
  local -ri argc="${#argv[@]}"

  if [[ ${argc} -ne 1 ]]; then
    error "Function ${FUNCNAME[0]} was given ${argc} auguments, but expected 1 augument."
  fi

  local -r nixpkgs_version="${argv[0]}"

  # Check if Home Manager installed
  set +e
  type -t home-manager >/dev/null 2>&1
  local -r exists="${?}"
  set -e

  if [[ "${exists}" -ne 0 ]]; then
    echo "Start installing Home Manager: using Nix."
    echo
    nix-channel --add "https://github.com/nix-community/home-manager/archive/release-${nixpkgs_version}.tar.gz" home-manager
    nix-channel --update
    export NIX_PATH="${HOME}/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}"
    nix-shell '<home-manager>' -A install
  fi
  echo "Home Manager installed."
  echo
}
