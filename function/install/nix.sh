#######################################
# Install Nix
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   $1: Nix Version
# Returns:
#   None
#######################################
install::nix() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

  if [[ ("${OSTYPE}" != "darwin"* && "${OSTYPE}" != "linux-gnu"*) ]]; then
    error "This Platform is not supported."
  fi

  if [[ ("${OSTYPE}" == "darwin"*) && ! -d /nix ]]; then
    echo ""
    echo "     ------------------------------------------------------------------ "
    echo "    | WARNING!!                                                        |"
    echo "     ------------------------------------------------------------------ "
    echo ""
    echo "    After macOS Catalina, the system volume is read only and cannot be"
    echo "    deployed directly to the root directory."
    echo ""
    echo "    There is a solution to create a symbolic link to the root directory"
    echo "    using synthetic.conf without disabling rootless, but also not merged"
    echo "    into the mainline of the Nix installer."
    echo ""
    echo "      https://github.com/NixOS/nix/issues/2925"
    echo ""
    echo ""
    echo "    Therefore, when using the installer of stable, it is necessary to"
    echo "    execute the following command to configure synthetic.conf and the"
    echo "    mountpoint before using it."
    echo ""
    echo "      curl -L https://raw.githubusercontent.com/NixOS/nix/d42ae78de9f92d60d0cd3db97216274946e8ba8e/scripts/create-darwin-volume.sh | sh"
    echo ""
    exit 1
  fi

  # Check if Nix installed
  set +e
  type -t nix > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    curl -o "/tmp/install-nix-${1}" "https://releases.nixos.org/nix/nix-${1}/install"
    curl -o "/tmp/install-nix-${1}.asc" "https://releases.nixos.org/nix/nix-${1}/install.asc"
    gpg --keyserver hkp://keys.gnupg.net --recv-keys B541D55301270E0BCF15CA5D8170B4726D7198DE
    gpg --verify "/tmp/install-nix-${1}.asc"
    sh "/tmp/install-nix-${1}"
  fi
}
