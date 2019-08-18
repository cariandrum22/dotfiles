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

# Import functions
source ./function/install/homebrew.sh
source ./function/install/fisher.sh
source ./function/install/stack.sh
source ./function/install/rustup.sh

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
  declare -a BASE_DOT_FILES=(
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

    declare -ar OS_SPECIFIC_DOT_FILES=(
      config/iTerm2/com.googlecode.iterm2.plist
      config/karabiner/karabiner.json
    )

    # Deploy configuration files into ~/Library/Application Support
    # TODO: Functionalize so that multiple files can be deployed
    cp -f "${ABS_PATH}/Library/Application Support/AquaSKK/keymap.conf" \
          "${HOME}/Library/Application Support/AquaSKK/keymap.conf"
  fi
  # TODO: Implement the function to install Homebrew

  # Install fisher
  install::fisher

  # Deploy dot files to ${HOME}
  declare -ar DOT_FILES=( "${BASE_DOT_FILES[@]}" "${OS_SPECIFIC_DOT_FILES[@]}")
  deploy_dot_files "${DOT_FILES[@]}"

  # Install arbitrary environment
  install::arbitrary_envs "${ARBENV_DEFINITIONS[@]}"

  # Install stack
  install::stack

  # Install rustup
  install::rustup
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
    launchctl load "${HOME}/Library/LaunchAgents/local.chroe.brew.plist"
  fi
  if [[ ! -e "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist" ]]; then
    cp -f "${ABS_PATH}/launchd/user/agents/local.chroe.google-ime-skk.plist" "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist"
    launchctl load "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist"
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
