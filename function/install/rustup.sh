#######################################
# Install rustup
# Globals:
#   HOME
#   PATH
# Arguments:
#   None
# Returns:
#   None
#######################################
install::rustup() {
  declare -x PATH="${HOME}/.cargo/bin:${PATH}"
  set +e
  type -t rustup > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    curl https://sh.rustup.rs -sSf | sh -s -- -y
  fi
}
