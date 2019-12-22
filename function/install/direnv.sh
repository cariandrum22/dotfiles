#######################################
# Install ghq
# Globals:
#   BASH_SOURCE
# Arguments:
#   None
# Returns:
#   Nonez
#######################################
install::direnv() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../go_resolver.sh"
  go_resolver

  # Check direnv installed
  set +e
  type -t direnv > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    go get -u github.com/direnv/direnv
  fi
}
