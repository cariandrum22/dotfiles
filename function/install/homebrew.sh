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
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

  if [[ "${OSTYPE}" != "darwin"* ]]; then
    error "This Platform is not supported."
  fi

  # Check if Homebrew installed
  set +e
  type -t brew
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    error "Homebrew are currenty installed."
  fi
}
