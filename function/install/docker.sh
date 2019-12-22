#######################################
# Install docker
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::docker() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../checked_gatekeeper_quarantine_flag.sh"

  # Check docker installed
  set +e
  type -t docker > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    if [[ "${OSTYPE}" == "linux-gnu" ]]; then
      error "Currently not implemented."
    elif [[ "${OSTYPE}" == "darwin"* ]]; then
      # Install docker-edge
      brew tap caskroom/versions
      brew cask install docker-edge

      # Check if docker is running
      set +e
      docker system info > /dev/null 2>&1
      local -r running="${?}"
      set -e
      if [[ "${running}" -ne 0 ]]; then
        checked_gatekeeper_quarantine_flag /Applications/Docker.app
        open --background -a Docker && \
          while ! docker system info > /dev/null 2>&1; do sleep 1; done && \
          docker run --rm busybox && \
          docker rmi busybox
      fi
    else
      error "This Platform is not supported."
    fi
  fi
}
