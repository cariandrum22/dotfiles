#!/usr/bin/env bash
# shellcheck source=function/error.sh
source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

#######################################
# Install Xcode Command Line Tools
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::xcode_command_line_tools() {
  if [[ "${OSTYPE}" != "darwin"* ]]; then
    error "This Platform is not supported."
  fi

  # Check if Xcode Command Line Tools installed
  set +e
  xcode-select -p >/dev/null 2>&1
  local -r exists="${?}"
  set -e

  local -i counter=0
  local -ir WAITING_TIME=3

  if [[ "${exists}" -ne 0 ]]; then
    echo "Start installing Xcode Command Line Tools."
    echo
    echo "Note: Follow the instructions of the launched installer to complete the installation. This script will wait until the Xcode Command Line Tools installation is complete."
    echo
    xcode-select --install >/dev/null 2>&1
    until xcode-select -p >/dev/null 2>&1; do
      sleep ${WAITING_TIME}
      ((counter += WAITING_TIME))
      echo "Waiting for Xcode installation to complete. ${counter} seconds elapsed."
    done
  fi
  echo "Xcode Command Line Tools installed."
  echo
}
