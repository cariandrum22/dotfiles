#!/bin/bash
#
# Perform initial setup of the environment.

# Initialize

# Treat unset variables
set -u
# Exit immediately if returns a non-zero status
set -e
# Get absolute path of this script
readonly abs_path="$(cd "$(dirname "${0}")"; pwd)"

# Import functions

# shellcheck disable=SC1091
source ./function/install/xcode_command_line_tools.sh
# shellcheck disable=SC1091
source ./function/install/homebrew.sh
# shellcheck disable=SC1091
source ./function/install/git.sh
# shellcheck disable=SC1091
source ./function/install/fisher.sh
# shellcheck disable=SC1091
source ./function/install/docker.sh
# shellcheck disable=SC1091
source ./function/install/ghq.sh
# shellcheck disable=SC1091
source ./function/install/direnv.sh
# shellcheck disable=SC1091
source ./function/install/arbitrary_envs.sh
# shellcheck disable=SC1091
source ./function/install/stack.sh
# shellcheck disable=SC1091
source ./function/install/rustup.sh
# shellcheck disable=SC1091
source ./function/install/nix.sh

# Set temporary PATH for installation
if [[ -z "${GOBIN:+UNDEF}" ]]; then
  readonly gobin="${GOPATH:-${HOME}/Go}/bin/$(uname -s | awk '{print tolower($0)}')_amd64"
  declare -x GOBIN="${gobin}"
  declare -x PATH="${GOBIN}:${PATH}"
fi

#######################################
# Entry point
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
main() {
  # Local variables

  # List of files to deploy
  # TODO: remove zsh related files
  local -a base_dot_files=(
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
  local -ar arbenv_definitions=(
    "rbenv/rbenv|true|rbenv/ruby-build"
    "nodenv/nodenv|true|nodenv/node-build"
    "pyenv/pyenv|true|pyenv/pyenv-virtualenv"
    "tokuhirom/plenv|false|tokuhirom/Perl-Build"
    "syndbg/goenv|false|"
  )

  # Run for macOS only
  if [ "$(uname)" == 'Darwin' ]; then
    install::xcode_command_line_tools
    install::homebrew
    deploy_launchd_agents

    local -ar os_specific_dot_files=(
      config/iTerm2/com.googlecode.iterm2.plist
      config/karabiner/karabiner.json
    )

    # Deploy configuration files into ~/Library/Application Support
    # TODO: Functionalize so that multiple files can be deployed
    if [[ -d "${HOME}/Library/Application Support/AquaSKK" ]]; then
      cp -f "${abs_path}/Library/Application Support/AquaSKK/keymap.conf" \
        "${HOME}/Library/Application Support/AquaSKK/keymap.conf"
    fi
  fi
  # TODO: Implement the function to install Homebrew

  # Install Git
  install::git

  # Install fisher
  install::fisher

  # Install Docker
  install::docker

  # Install ghq
  install::ghq

  # Install direnv
  install::direnv

  # Deploy dot files to ${HOME}
  local -ar dot_files=( "${base_dot_files[@]}" "${os_specific_dot_files[@]}")
  deploy_dot_files "${dot_files[@]}"

  # Install arbitrary environment
  install::arbitrary_envs "${arbenv_definitions[@]}"

  # Install stack
  install::stack

  # Install rustup
  install::rustup

  # Install Nix
  install::nix
}

#######################################
# Deploy dot files to ${HOME}
# Globals:
#   HOME
# Arguments:
#   dot_files::Array
# Returns:
#   None
#######################################
deploy_dot_files() {
  # TODO: Implement error handling related to arguments and dependencies
  local -ar dot_files=("${@}")

  prepare_sub_dir() {
    local -ar dot_files=("${@}")

    for file in "${dot_files[@]}"; do
      if [[ "${file}" =~ [:word:]*/[:word:]* ]]; then
        mkdir -p "${HOME}/.$(dirname "${file}")"
      fi
    done
  }

  prepare_sub_dir "${dot_files[@]}"

  for file in "${dot_files[@]}"; do
    ln -sfn "${abs_path}"/"${file}" "${HOME}"/."${file}"
  done
}

#######################################
# Deploy launchd agents
# Globals:
#   HOME
#   PATH
#   abs_path
# Arguments:
#   None
# Returns:
#   None
#######################################
 deploy_launchd_agents() {
  # For User Agents
  if [[ ! -f "${HOME}/Library/LaunchAgents/local.chroe.brew.plist" ]]; then
    if [[ ! -d "${HOME}/Library/LaunchAgents" ]]; then
      mkdir -m 700 "${HOME}/Library/LaunchAgents"
    fi
    cp -f "${abs_path}/launchd/user/agents/local.chroe.brew.plist" "${HOME}/Library/LaunchAgents/local.chroe.brew.plist"
    launchctl load "${HOME}/Library/LaunchAgents/local.chroe.brew.plist"
  fi
  if [[ ! -f "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist" ]]; then
    cp -f "${abs_path}/launchd/user/agents/local.chroe.google-ime-skk.plist" "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist"
    launchctl load "${HOME}/Library/LaunchAgents/local.chroe.google-ime-skk.plist"
  fi

  # For Global Agents
  if [[ ! -f /Library/LaunchAgents/local.chroe.locate.updatedb.plist ]]; then
    echo "Install laundhd Global Agent."
    echo "Please input sudo password."
    sudo cp -f "${abs_path}/launchd/global/agents/local.chroe.locate.updatedb.plist" /Library/LaunchAgents/local.chroe.locate.updatedb.plist
    sudo launchctl load /Library/LaunchAgents/local.chroe.locate.updatedb.plist
  fi
}

# Run main
main
