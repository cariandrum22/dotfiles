#!/bin/bash
#
# Perform initial setup of the environment.

# Initialize

# Treat unset variables
set -u
# Exit immediately if returns a non-zero status
set -e
# Get absolute path of this script
ABS_PATH="$(cd "$(dirname "${0}")"; pwd)"
declare -r ABS_PATH

#######################################
# Entry point
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function main () {
  # Local variables
  
  # List of files to deploy
  # TODO: remove zsh related files 
  declare -ar DOT_FILES=(
    # zshrc
    # zshrc.personal
    # zlogin
    # zsh_functions
    gitignore
    gitconfig
    emacs.el
    emacs.d
    Xresources
    pythonstartup
    gemrc
    bundle
    rubocop.yml
    config/fish/config.fish
    config/fish/fishfile
    config/iTerm2/com.googlecode.iterm2.plist
  )

  # List of arbitrary environment (e.g. rbenv, nodenv, pyenv, etc.)
  #   Variable Format:
  #     Env(Enter the path of GitHub) | Make it?(Boolean) | Plugins(Enter the path of GitHub as colon separated value)
  #   Example:
  #     rbenv|true|rbenv/ruby-build:rkh/rbenv-update:jf/-rbenv-gemset
  declare -ar ARBENV_DEFINITIONS=(
    "rbenv/rbenv|true|rbenv/ruby-build"
    "nodenv/nodenv|true|nodenv/node-build"
    "pyenv/pyenv|true|pyenv/pyenv-virtualenv"
    "tokuhirom/plenv|false|tokuhirom/Perl-Build"
    "syndbg/goenv|false|"
  )
  
  # Run for macOS only
  if [ "$(uname)" == 'Darwin' ]; then
    install_homebrew
    deploy_launchd_agents
  fi
  # TODO: Implement the function to install Homebrew
  
  # Deploy dot files to ${HOME}
  deploy_dot_files "${DOT_FILES[@]}"
  
  # Install arbitrary environment
  install_arbitrary_envs "${ARBENV_DEFINITIONS[@]}"
  
  # Install rustup
  install_rustup
}

#######################################
# Deploy dot files to ${HOME}
# Globals:
#   HOME
# Arguments:
#   DOT_FILES::Array
# Returns:
#   None
#######################################
function deploy_dot_files () {
  # TODO: Implement error handling related to arguments and dependencies
  declare -ar DOT_FILES=("${@}")

  function prepare_sub_dir () {
    declare -ar DOT_FILES=("${@}")

    for file in "${DOT_FILES[@]}"; do
      if [[ "${file}" =~ [:word:]*/[:word:]* ]]; then
        mkdir -p "${HOME}/.$(dirname "${file}")"
      fi
    done
  }
  
  prepare_sub_dir "${DOT_FILES[@]}"  

  for file in "${DOT_FILES[@]}"; do
    ln -sfn "${ABS_PATH}"/"${file}" "${HOME}"/."${file}"
  done
}

#######################################
# Deploy dot files to ${HOME}
# Globals:
#   HOME
#   PATH
# Arguments:
#   ARBENV_DEFINITIONS::Array
# Returns:
#   None
#######################################
function install_arbitrary_envs () {
  # TODO: Implement error handling related to arguments and dependencies
  declare -ar ARBENV_DEFINITIONS=("${@}")

  function install_env () {
    declare -ar ARBENV_DEFINITION=($(echo "${arbenv_definition}" | tr -s '|' ' '))

    declare -r ENV="${ARBENV_DEFINITION[0]}"
    env_basename="$(basename "${ENV}")"
    declare -r ENV_BASENAME=${env_basename}
    declare -r MAKE_IT="${ARBENV_DEFINITION[1]}"
    if [[ ${#ARBENV_DEFINITION[*]} -gt 2 ]]; then
      declare -r PLUGINS="${ARBENV_DEFINITION[2]}"
    fi

    declare -r INSTALL_PATH="${HOME}"/."${ENV_BASENAME}"
    
    if [[ ! -d "${INSTALL_PATH}" ]]; then
      git clone https://github.com/"${ENV}".git "${INSTALL_PATH}"
      
      if "${MAKE_IT}"; then
        cd "${INSTALL_PATH}" && src/configure && make -C src
      fi
    fi

    declare -x PATH="${INSTALL_PATH}/bin:${PATH}"

    function install_plugin () {
      declare -r PLUGIN="${1}"
      env_root_dir=$("${ENV_BASENAME}" root)
      declare -r ENV_ROOT_DIR=${env_root_dir}
      plugin_basename=$(basename "${PLUGIN}")
      declare -r PLUGIN_INSTALL_PATH="${ENV_ROOT_DIR}"/plugins/"${plugin_basename}"
        
      if [[ ! -d "${PLUGIN_INSTALL_PATH}" ]]; then
        git clone https://github.com/"${PLUGIN}".git "${PLUGIN_INSTALL_PATH}"
      fi
    }

    if [[ -n ${PLUGINS-} ]]; then
      declare -ar SPLITED_PLUGINS=($(echo "${PLUGINS}" | tr -s ':' ' '))
      
      for plugin in "${SPLITED_PLUGINS[@]}"; do
        install_plugin "${plugin}"
      done
    fi
  }
  
  for arbenv_definition in "${ARBENV_DEFINITIONS[@]}"; do
    install_env "${arbenv_definition}"
  done
}

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
function install_rustup () {
  declare -x PATH="${HOME}/.cargo/bin:${PATH}"
  set +e
  type rustup 1>/dev/null
  declare -ir EXISTS="${?}"
  set -e
  if [[ "${EXISTS}" -ne 0 ]]; then
    curl https://sh.rustup.rs -sSf | sh
  fi
}

#######################################
# Install Homebrew
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function install_homebrew () {
  set +e
  type brew 1>/dev/null
  declare -ir EXISTS="${?}"
  set -e
  if [[ "${EXISTS}" -ne 0 ]]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi
}

#######################################
# Deploy launchd agents
# Globals:
#   HOME
#   PATH
# Arguments:
#   None
# Returns:
#   None
#######################################
function deploy_launchd_agents () {
  # For User Agents
  if [[ ! -e "${HOME}/Library/LaunchAgents/local.chroe.brew.plist" ]]; then
    cp -f "${ABS_PATH}/launchd/user/agents/local.chroe.brew.plist" "${HOME}/Library/LaunchAgents/local.chroe.brew.plist"
    launchctl load /Library/LaunchAgents/local.chroe.locate.updatedb.plist
  fi
  # For Global Agents
  if [[ ! -e /Library/LaunchAgents/local.chroe.locate.updatedb.plist ]]; then
    echo "Install laundhd Global Agent."
    echo "Please input sudo password."
    sudo cp -f "${ABS_PATH}/launchd/global/agents/local.chroe.locate.updatedb.plist" /Library/LaunchAgents/local.chroe.locate.updatedb.plist
    sudo launchctl load /Library/LaunchAgents/local.chroe.locate.updatedb.plist
  fi
}

# Run main
main
