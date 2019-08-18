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
  type -t rustup
  local -r EXISTS="${?}"
  set -e
  if [[ "${EXISTS}" -ne 0 ]]; then
    curl https://sh.rustup.rs -sSf | sh -s -- -y
  fi
}
