#######################################
# Install Xcode Command Line Tools
# Globals:
#   BASH_SOURCE
#   OSTYPE
# Arguments:
#   None
# Returns:
#   None
#######################################
install::xcode_command_line_tools() {
  # shellcheck source=/dev/null
  source "$(dirname "${BASH_SOURCE[0]}")/../error.sh"

  if [[ "${OSTYPE}" != "darwin"* ]]; then
    error "This Platform is not supported."
  fi

  # Check if Xcode Command Line Tools installed
  set +e
  xcode-select -p > /dev/null 2>&1
  local -r exists="${?}"
  set -e
  if [[ "${exists}" -ne 0 ]]; then
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    command_line_tools=$(softwareupdate -l \
        | grep "\*.*Command Line" \
        | head -n 1 \
        | awk -F"*" '{print $2}' \
        | sed -e 's/^[[:blank:]]*//' -e 's/[[:blank:]]*$//')
    softwareupdate -i "${command_line_tools}" --verbose
    rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
  fi
}
