#######################################
# go resolver
# Globals:
#   BASH_SOURCE
#   GOPATH
#   HOME
# Arguments:
#   None
# Returns:
#   None
#######################################
go_resolver() {
  # Check go installed
  set +e
  type -t go > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")/go.sh"
  fi
}
