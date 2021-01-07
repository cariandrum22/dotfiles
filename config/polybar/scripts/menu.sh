#!/usr/bin/env bash

readonly MENU_SCRIPT=$(realpath -s "$0")
readonly MENU_SCRIPT_PATH=$(dirname "$MENU_SCRIPT")

# shellcheck disable=SC1090
. "$MENU_SCRIPT_PATH/colors.sh"

readonly FONT="Fira Code 12"
# shellcheck disable=SC2154
rofi -show drun \
  -no-lazy-grab \
  -modi drun \
  -width 45 \
  -lines 15 \
  -columns 2 \
  -font "$FONT" \
  -bw 0 \
  -location 1 \
  -padding 20 \
  -xoffset 10 \
  -yoffset 40 \
  -show-icons \
  -drun-display-format "{name}" \
  -line-padding 10 \
  -color-window "${colors[nord0]},${colors[nord0]},${colors[nord0]}" \
  -color-normal "${colors[nord1]},${colors[nord4]},${colors[nord1]},${colors[nord9]},${colors[nord6]}" \
  -color-urgent "${colors[nord1]},${colors[nord11]},${colors[nord1]},${colors[nord11]},${colors[nord6]}" \
  -color-active "${colors[nord1]},${colors[nord9]},${colors[nord1]},${colors[nord9]},${colors[nord6]}" \
  -display-drun "Applications"
