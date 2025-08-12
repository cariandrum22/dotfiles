#!/usr/bin/env bash
# shellcheck source=function/error.sh
source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

#######################################
# Load Nix Profile
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
load_profile() {
  if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    set +u
    # shellcheck source=/dev/null
    source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    set -u
  fi
}

#######################################
# Install Nix
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::nix() {
  if [[ ("${OSTYPE}" != "darwin"* && "${OSTYPE}" != "linux-gnu"*) ]]; then
    error "This Platform is not supported."
  fi

  load_profile

  # Check if Nix installed
  set +e
  type -t nix >/dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    echo "Start installing Nix: the package manager."
    echo
    sh <(curl -L https://nixos.org/nix/install)
    load_profile
  fi
  echo "Nix installed."
  echo
}
