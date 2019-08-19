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
  local -r install_path="${HOME}"/.config/fish/functions/fisher.fish
  if [[ ! -d "${install_path}" ]]; then
    curl https://git.io/fisher --create-dirs -sLo "${install_path}"
  fi
}
