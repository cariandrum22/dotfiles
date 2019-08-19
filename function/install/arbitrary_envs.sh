#######################################
# Install arbitrary envs
# Globals:
#   HOME
#   PATH
# Arguments:
#   arbenv definitions::Array
# Returns:
#   None
#######################################
install::arbitrary_envs() {
  # TODO: Implement error handling related to arguments and dependencies
  local -ar arbenv_definitions=("${@}")

  install::env() {
    local -a arbenv_definition
    IFS="|" read -r -a arbenv_definition <<< "${@}"

    local -r env="${arbenv_definition[0]}"
    local -r env_basename="$(basename "${env}")"
    local -r make_it="${arbenv_definition[1]}"
    if [[ ${#arbenv_definition[*]} -gt 2 ]]; then
      local -r plugins="${arbenv_definition[2]}"
    fi

    local -r install_path="${HOME}"/."${env_basename}"

    if [[ ! -d "${install_path}" ]]; then
      git clone https://github.com/"${ENV}".git "${install_path}"

      if "${make_it}"; then
        cd "${install_path}" && src/configure && make -C src
      fi
    fi

    declare -x PATH="${install_path}/bin:${PATH}"

    install::plugin() {
      local -r plugin="${1}"
      local -r env_root_dir=$("${env_basename}" root)
      plugin_basename=$(basename "${plugin}")
      local -r plugin_install_path="${env_root_dir}"/plugins/"${plugin_basename}"

      if [[ ! -d "${plugin_install_path}" ]]; then
        git clone https://github.com/"${plugin}".git "${plugin_install_path}"
      fi
    }

    if [[ -n ${plugins-} ]]; then
      local -a splited_plugins
      IFS=":" read -r -a splited_plugins <<< "${plugins}"

      for plugin in "${splited_plugins[@]}"; do
        install::plugin "${plugin}"
      done
    fi
  }

  for arbenv_definition in "${arbenv_definitions[@]}"; do
    install::env "${arbenv_definition}"
  done
}
