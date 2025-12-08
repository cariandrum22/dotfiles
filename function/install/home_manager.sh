#!/usr/bin/env bash
# shellcheck source=function/error.sh
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
    echo "Start installing Home Manager: using Nix flakes."
    echo
    # Install home-manager using nix profile (flakes way)
    nix profile install github:nix-community/home-manager

    # For compatibility, also add the channel (but it won't be used with flakes)
    nix-channel --add "https://github.com/nix-community/home-manager/archive/release-${nixpkgs_version}.tar.gz" home-manager
    nix-channel --update
  fi
  echo "Home Manager installed."
  echo
}
