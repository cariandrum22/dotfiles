#######################################
# Install Fisher
# Globals:
#   HOME
# Arguments:
#   None
# Returns:
#   None
#######################################
install::fisher() {
  local -r INSTALL_PATH="${HOME}"/.config/fish/functions/fisher.fish
  if [[ ! -d "${INSTALL_PATH}" ]]; then
    curl https://git.io/fisher --create-dirs -sLo "${INSTALL_PATH}"
  fi
}
