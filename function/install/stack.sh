#######################################
# Install Stack
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
install::stack() {
  set +e
  type -t stack > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    curl -sSL https://get.haskellstack.org/ | sh
  fi
}
