#!/usr/bin/env bash
#######################################
# Checked the Gatekeeper quarantine flag
# Globals:
#   None
# Arguments:
#   Application Path::array
# Returns:
#   None
#######################################
checked_gatekeeper_quarantine_flag() {
  local -r app_attr="$(xattr -l "${1}")"
  local -r app_attr_name="$(echo -n "${app_attr}" | awk '{print $1}' | sed -e 's/:$//')"
  local -r app_attr_value="$(echo -n "${app_attr}" | awk '{print $2}')"

  IFS=';' read -r -a app_attr_values <<<"${app_attr_value}"

  local -r checked_flag="$(echo "obase=16; ibase=2; 01000000" | bc)"
  local -r checked_flag_applied="$(echo "obase=16; ibase=16; ${app_attr_values[0]}+${checked_flag}" | bc)"

  app_attr_values[0]="$(printf %04x 0x"${checked_flag_applied}")"
  local -r new_app_attr_values="$(
    IFS=,
    echo "${app_attr_values[@]}"
  )"

  xattr -w "${app_attr_name}" "${new_app_attr_values}" "${1}"
}
