#!/usr/bin/env bash

readonly NIXPKGS_VERSION="23.05"

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
# Deploy local/bin files to ${HOME}/.local/bin
# Globals:
#   HOME
# Arguments:
#   None
# Returns:
#   None
#######################################
deploy_local_bin_files() {
  local -r bin_dir="${HOME}/.local/bin"

  if [[ ! -d "${bin_dir}" ]]; then
    mkdir -p "${bin_dir}"
  fi

  local -a bin_files=()
  while IFS= read -r file; do
    bin_files+=("${file}")
  done < <(find "${abs_path}/local/bin" -type f -not -path "*/\.*")

  for bin_file in "${bin_files[@]}"; do
    local file_name
    file_name=$(basename "${bin_file}")
    ln -sfn "${bin_file}" "${bin_dir}/${file_name}"
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
    config/home-manager
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
    # For Apple Silicon devices, applications installed with Homebrew may require Rosetta2, so install it
    if [ "$(uname -a)" == 'arm64' ]; then
      sudo softwareupdate --install-rosetta --agree-to-license
    fi
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

  # Delete existing ${HOME}/.config/home-manager directory to replace it with a file under git management
  rm -rf "${HOME}/.config/home-manager"

  # Deploy dotfiles to ${HOME}
  deploy_dotfiles "${dotfiles[@]}"

  # Deploy ${HOME}/.local/bin files
  deploy_local_bin_files

  # Reflecting the configuration under home-manager management
  add_nix_channels "${nix_channels[@]}"
  home-manager switch

  # Install fisher and fish plugins
  install::fisher

  echo "Setup is complete."
}

# Run
main
