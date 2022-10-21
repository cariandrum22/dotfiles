#!/usr/bin/env bash
#######################################
# error
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
error() {
  echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')]: ${*}" >&2
  exit 1
}
