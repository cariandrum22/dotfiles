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
# shellcheck source=./function/detect_system.sh
source ./function/detect_system.sh
# shellcheck source=./function/install/xcode_command_line_tools.sh
source ./function/install/xcode_command_line_tools.sh
# shellcheck source=./function/install/homebrew.sh
source ./function/install/homebrew.sh
# shellcheck source=./function/install/nix.sh
source ./function/install/nix.sh
# shellcheck source=./function/install/home_manager.sh
source ./function/install/home_manager.sh
# shellcheck source=./function/install/fisher.sh
source ./function/install/fisher.sh
# shellcheck source=./function/add_nix_channels.sh
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
    config/fish/fish_plugins
  )

  # List of nix-channels to add
  local -a nix_channels=()

  # Run only on macOS
  if is_darwin; then
    install::xcode_command_line_tools
    install::homebrew

    nix_channels+=(
      "nixpkgs=https://nixos.org/channels/nixpkgs-${NIXPKGS_VERSION}-darwin"
      "unstable=https://nixos.org/channels/nixpkgs-unstable"
    )

    echo "Install a package under homebrew management."
    echo
    # For Apple Silicon devices, applications installed with Homebrew may require Rosetta2, so install it
    if is_apple_silicon; then
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

  # Deploy dotfiles to ${HOME}
  deploy_dotfiles "${dotfiles[@]}"

  # Deploy ${HOME}/.local/bin files
  deploy_local_bin_files

  # Reflecting the configuration under home-manager management
  add_nix_channels "${nix_channels[@]}"

  # Detect system and determine flake target
  local system
  system="$(detect_system)" || exit 1

  local flake_target
  flake_target="$(get_flake_target)" || exit 1

  # Show informative messages
  if [ -n "${DOTFILES_GUI:-}" ]; then
    if [ "${DOTFILES_GUI}" = "false" ] || [ "${DOTFILES_GUI}" = "0" ]; then
      echo "Using headless configuration (manually specified)"
    else
      echo "Using GUI configuration (manually specified)"
    fi
  elif is_linux; then
    if is_gui_system; then
      echo "GUI detected, using desktop configuration"
    else
      echo "No GUI detected, using headless configuration"
    fi
  fi

  echo "Detected system: ${system}"
  echo "Using configuration: ${flake_target}"

  # Switch to the appropriate home-manager configuration using flake
  home-manager switch --flake "${abs_path}#${flake_target}" --impure

  # Install fisher and fish plugins
  install::fisher

  echo "Setup is complete."
}

# Run
main
