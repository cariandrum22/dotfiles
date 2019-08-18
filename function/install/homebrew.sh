#######################################
# Install Homebrew
# Globals:
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::homebrew() {
  source ../error.sh

  if [[ "${OSTYPE}" != "darwin"* ]]; then
    error "This Platform is not supported."
  fi

  # Check if Homebrew installed
  set +e
  type -t brew
  local -r EXISTS="${?}"
  set -e
  if [[ "${EXISTS}" -ne 0 ]]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    error "Homebrew are currenty installed."
  fi
}
