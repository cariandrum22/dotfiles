#!/usr/bin/env bash
# shellcheck source=function/error.sh
source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

#######################################
# Eval Hoemebrew shellenv
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
eval_shellenv() {
  if [ -e '/opt/homebrew/bin/brew' ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi
}

#######################################
# Install Homebrew
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::homebrew() {
  if [[ "${OSTYPE}" != "darwin"* ]]; then
    error "This Platform is not supported."
  fi

  eval_shellenv

  # Check if Homebrew installed
  set +e
  type -t brew >/dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    echo "Start installing Homebrew."
    echo
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval_shellenv
  fi
  echo "Homebrew installed."
  echo
}
