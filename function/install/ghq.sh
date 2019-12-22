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
  source "$(dirname "${BASH_SOURCE[0]}")/../go_resolver.sh"
  go_resolver

  # Check ghq installed
  set +e
  type -t ghq > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    go get -u github.com/motemen/ghq
  fi
}
