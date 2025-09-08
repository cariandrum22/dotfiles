#!/usr/bin/env bash

#######################################
# Detect system architecture and OS
# Globals:
#   None
# Arguments:
#   None
# Outputs:
#   Writes system type to stdout (e.g., x86_64-linux, aarch64-darwin)
# Returns:
#   0 on success, 1 on unsupported system
#######################################
detect_system() {
  local arch
  arch="$(uname -m)"
  local os
  os="$(uname -s)"
  local system=""
  
  if [ "${os}" == "Darwin" ]; then
    if [ "${arch}" == "arm64" ]; then
      system="aarch64-darwin"
    else
      system="x86_64-darwin"
    fi
  elif [ "${os}" == "Linux" ]; then
    if [ "${arch}" == "aarch64" ]; then
      system="aarch64-linux"
    else
      system="x86_64-linux"
    fi
  else
    echo "Unsupported system: ${os} ${arch}" >&2
    return 1
  fi
  
  echo "${system}"
}

#######################################
# Check if the system is Darwin/macOS
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   0 if Darwin, 1 otherwise
#######################################
is_darwin() {
  [ "$(uname -s)" == "Darwin" ]
}

#######################################
# Check if the system is Linux
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   0 if Linux, 1 otherwise
#######################################
is_linux() {
  [ "$(uname -s)" == "Linux" ]
}

#######################################
# Check if the system is Apple Silicon (arm64)
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   0 if arm64 Darwin, 1 otherwise
#######################################
is_apple_silicon() {
  [ "$(uname -s)" == "Darwin" ] && [ "$(uname -m)" == "arm64" ]
}

#######################################
# Check if the system has GUI (for Linux)
# Globals:
#   DOTFILES_GUI (optional override)
# Arguments:
#   None
# Returns:
#   0 if GUI system, 1 if headless
#######################################
is_gui_system() {
  # Only check on Linux
  if ! is_linux; then
    return 0  # macOS always has GUI
  fi
  
  # Allow manual override with DOTFILES_GUI environment variable
  if [ -n "${DOTFILES_GUI:-}" ]; then
    if [ "${DOTFILES_GUI}" = "false" ] || [ "${DOTFILES_GUI}" = "0" ]; then
      return 1  # Headless
    else
      return 0  # GUI
    fi
  fi
  
  # Auto-detect GUI/headless
  # Check if systemd graphical.target is active (most reliable)
  if command -v systemctl >/dev/null 2>&1; then
    if systemctl is-active graphical.target >/dev/null 2>&1; then
      return 0  # GUI
    fi
  fi
  
  # Check for X11 or Wayland processes
  if pgrep -x "Xorg|Xwayland|wayland" >/dev/null 2>&1; then
    return 0  # GUI
  fi
  
  # Check for display environment variables
  if [ -n "${WAYLAND_DISPLAY:-}" ] || [ -n "${DISPLAY:-}" ]; then
    return 0  # GUI
  fi
  
  return 1  # Headless
}

#######################################
# Get the appropriate flake target for the system
# Globals:
#   DOTFILES_GUI (optional override)
# Arguments:
#   None
# Outputs:
#   Writes flake target to stdout (e.g., x86_64-linux, x86_64-linux-headless)
# Returns:
#   0 on success, 1 on error
#######################################
get_flake_target() {
  local system
  system="$(detect_system)" || return 1
  
  # Check if headless configuration should be used
  if is_linux && ! is_gui_system; then
    echo "${system}-headless"
  else
    echo "${system}"
  fi
}