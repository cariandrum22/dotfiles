#######################################
# Install ghq
# Globals:
#   none
# Arguments:
#   None
# Returns:
#   None
#######################################
install::ghq() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../go.sh"

  # Check ghq installed
  set +e
  type -t ghq
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    go get github.com/motemen/ghq
  fi
}
