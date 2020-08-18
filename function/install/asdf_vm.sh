#######################################
# Install asdf-vm
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   $1: asdf-vm Version
# Returns:
#   None
#######################################
install::asdf_vm() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

  # Check asdf-vm installed
  set +e
  type -t asdf > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch "${1}"
  fi
}
