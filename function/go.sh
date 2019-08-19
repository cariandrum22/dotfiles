#######################################
# go
# Globals:
#   BASH_SOURCE
#   GOPATH
#   HOME
# Arguments:
#   None
# Returns:
#   None
#######################################
go() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/error.sh"

  # Check Docker installed
  set +e
  type -t docker
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    error "docker command not found."
  else
    docker run --rm -v "${GOPATH:-${HOME}/Go}":/go -e GOOS="$(uname -s | awk '{print tolower}')" -e GOARCH=amd64 golang:stretch go "${@}"
  fi
}
