#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/error.sh"

#######################################
# Add nix channel
# Globals:
#   FUNCNAME
# Arguments:
#   URL
#   Name
# Returns:
#   None
#######################################
add_nix_channel() {
  local -ra argv=("${@}")
  local -ri argc="${#argv[@]}"

  if [[ ${argc} -ne 2 ]]; then
    error "Function ${FUNCNAME[0]} was given ${argc} auguments, but expected 2 auguments."
  fi

  local -r url="${argv[0]}"
  local -r name="${argv[1]}"

  # Check if unstable exists in the list of nix-channel
  set +e
  nix-channel --list | awk '{print $1}' | grep "${name}" >/dev/null 2>&1
  local -r channel_exists="${?}"
  set -e
  if [[ "${channel_exists}" -ne 0 ]]; then
    # Check if the path to the channel exists in the home-manager configurations
    set +e
    find "${HOME}/.config/nixpkgs/" -type f -name "*.nix" -print0 | xargs -0 grep '<'"${name}"'>' >/dev/null 2>&1
    local -r detect_channel_path_within_nix_files="${?}"
    set -e
    if [[ "${detect_channel_path_within_nix_files}" -eq 0 ]]; then
      echo "Add nix channel: ${name}"
      echo
      nix-channel --add "${url}" "${name}"
      nix-channel --update
      echo
      echo "nix channel: ${name} have been added."
      echo
    fi
  fi
}

#######################################
# Add nix channels
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   nix_channels::Quasi associative array(equal separated)
# Returns:
#   None
#######################################
add_nix_channels() {
  if [[ ("${OSTYPE}" != "darwin"* && "${OSTYPE}" != "linux-gnu"*) ]]; then
    error "This Platform is not supported."
  fi

  local -ra nix_channels=("${@}")

  # Check if Nix installed
  set +e
  type -t nix >/dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -eq 0 ]]; then
    for ch in "${nix_channels[@]}"; do
      name=$(echo "${ch}" | awk -F'[=]' '{print $1}')
      url=$(echo "${ch}" | awk -F'[=]' '{print $2}')
      add_nix_channel "${url}" "${name}"
    done
  fi
}
