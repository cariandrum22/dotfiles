#!/usr/bin/env bash

readonly NIXPKGS_VERSION="22.05"

# Environment Setup Script.
echo "Start setup."
echo

# Treat unset variables
set -u
# Immediately exit if it returns a non-zero status.
set -e

# Get absolute path of this script
abs_path="$(
  cd "$(dirname "${0}")"
  pwd
)"
readonly abs_path

# Import functions
source ./function/install/xcode_command_line_tools.sh
source ./function/install/homebrew.sh
source ./function/install/nix.sh
source ./function/install/home_manager.sh
source ./function/install/fisher.sh
source ./function/add_nix_channels.sh

#######################################
# Deploy dot files to ${HOME}
# Globals:
#   HOME
# Arguments:
#   dotfiles::Array
# Returns:
#   None
#######################################
deploy_dotfiles() {
  # TODO: Implement error handling related to arguments and dependencies
  local -ra dotfiles=("${@}")

  prepare_sub_dir() {
    local -ra dotfiles=("${@}")

    for file in "${dotfiles[@]}"; do
      if [[ "${file}" =~ [:word:]*/[:word:]* ]]; then
        mkdir -p "${HOME}/.$(dirname "${file}")"
      fi
    done
  }

  prepare_sub_dir "${dotfiles[@]}"

  for dotfile in "${dotfiles[@]}"; do
    ln -sfn "${abs_path}"/"${dotfile}" "${HOME}"/."${dotfile}"
  done
}

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
  # List of dotfiles to deploy
  local -a dotfiles=(
    config/nixpkgs
    config/fish/fish_plugins
  )

  # List of nix-channels to add
  local -a nix_channels=()

  # Run only on macOS
  if [ "$(uname)" == 'Darwin' ]; then
    install::xcode_command_line_tools
    install::homebrew

    nix_channels+=(
      "nixpkgs=https://nixos.org/channels/nixpkgs-${NIXPKGS_VERSION}-darwin"
      "unstable=https://nixos.org/channels/nixpkgs-unstable"
    )

    echo "Install a package under homebrew management."
    echo
    # Install Rosetta 2 in advance because the application installed by brew requires it
    sudo softwareupdate --install-rosetta --agree-to-license
    brew bundle --file="${abs_path}"/Brewfile
    echo
  fi

  # Run only on non-NixOS
  if ! type -t nixos-version >/dev/null 2>&1; then
    dotfiles+=(config/nix)
  fi

  # Install Nix
  install::nix

  # Install Home Manager
  install::home_manager "${NIXPKGS_VERSION}"

  # Delete existing ${HOME}/.config/nixpkgs directory to replace it with a file under git management
  rm -rf "${HOME}/.config/nixpkgs"

  # Deploy dot files to ${HOME}
  deploy_dotfiles "${dotfiles[@]}"

  # Reflecting the configuration under home-manager management
  add_nix_channels "${nix_channels[@]}"
  home-manager switch

  # Install fisher and fish plugins
  install::fisher

  echo "Setup is complete."
}

# Run
main
