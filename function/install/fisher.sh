#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

#######################################
# Install fisher and fish plugins
# Globals:
#   BASH_SOURCE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::fisher() {
  # Check if fish installed
  set +e
  type -t fish >/dev/null 2>&1
  local -r exists="${?}"
  set -e

  if [[ "${exists}" -eq 0 ]]; then
    echo "Start installing/updating fisher and fish plugins"
    echo
    fish -c "source (curl -sL https://git.io/fisher | psub) && fisher update"
    echo "fisher and fish plugins installed/updated."
    echo
  else
    error "fish not found."
  fi
}
