#######################################
# Install Git
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::git() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

  # Check Git installed
  set +e
  type -t git > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    if [[ "${OSTYPE}" == "linux-gnu" ]]; then
      error "Currently not implemented."
    elif [[ "${OSTYPE}" == "darwin"* ]]; then
      # Install Git
      brew install git
    fi
  fi
}
