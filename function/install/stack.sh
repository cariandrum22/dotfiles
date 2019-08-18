#######################################
# Install Stack
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function install::stack () {
  set +e
  type -t stack
  local -r EXISTS="${?}"
  set -e
  if [[ "${EXISTS}" -ne 0 ]]; then
    curl -sSL https://get.haskellstack.org/ | sh
  fi
}
