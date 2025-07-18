# dotfiles

[![CI](https://github.com/cariandrum22/dotfiles/actions/workflows/ci.yml/badge.svg)](https://github.com/cariandrum22/dotfiles/actions/workflows/ci.yml)

Personal system configuration using Nix and Home Manager.

## Features

- Declarative configuration management with Nix Flakes
- Cross-platform support for macOS (Apple Silicon) and Linux
- Automated setup for tools and applications
- Pre-configured shells, editors, and utilities

## Supported Platforms

- x86_64-linux
- aarch64-darwin (Apple Silicon Macs)

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- Git

Note: Home Manager will be installed automatically during setup.

## Installation

```bash
# Clone the repository
git clone https://github.com/cariandrum22/dotfiles.git
cd dotfiles

# Run the setup script (installs Nix, Home Manager, and other dependencies)
bash setup.sh

# Apply Home Manager configuration
cd config/home-manager
nix run . -- switch
```

## Usage

### Update Configuration

```bash
# Update flakes (run in each directory)
nix flake update
cd config/home-manager && nix flake update
cd xmonad && nix flake update

# Apply changes
cd config/home-manager
nix run . -- switch
```

### Working with the Repository

```bash
# Enter shell with linting and formatting tools
nix develop
```

## Structure

### Core Configuration

- `flake.nix` - Top-level flake for development environment and pre-commit hooks
- `setup.sh` - Initial setup script for installing Nix, Home Manager, and dependencies

### Home Manager

- `config/home-manager/` - Main user environment configuration
  - `flake.nix` - Home Manager flake with platform detection
  - `home.nix` - Base configuration entry point
  - `programs/` - Application-specific configurations (Git, Emacs, VS Code, etc.)
  - `services/` - System services (Dunst, GPG agent, Picom, etc.)
  - `home/packages/` - Package lists organized by platform

### Window Management

- `xmonad/` - XMonad window manager configuration (Linux only)
  - Independent flake for Haskell-based window manager
- `config/polybar/` - Status bar configuration for Linux

### Shell and Terminal

- `config/fish/` - Fish shell configuration and plugins
- `emacs.d/` - GNU Emacs configuration files
- `Library/Preferences/` - iTerm2 settings (macOS)

### macOS Specific

- `Brewfile` - Homebrew package definitions for macOS applications

### Utilities

- `scripts/` - Helper scripts for updating VS Code extensions
- `function/` - Setup and installation helper functions

## Additional Notes

### iTerm2 Configuration (macOS)

The iTerm2 configuration file is included but not automatically deployed. To use it:

1. Open iTerm2 Preferences > General > Preferences
2. Enable "Load preferences from a custom folder or URL"
3. Select `Library/Preferences/com.googlecode.iterm2.plist` from this repository

## License

MIT License
