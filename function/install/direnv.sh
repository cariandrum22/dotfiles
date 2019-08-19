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
  source "$(dirname "${BASH_SOURCE[0]}")/../go.sh"

  # Check direnv installed
  set +e
  type -t direnv
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    go get github.com/direnv/direnv
  fi
}
