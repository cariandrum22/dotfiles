#!/usr/bin/env bash

readonly POWER_SCRIPT=$(realpath -s "$0")
readonly POWER_SCRIPT_PATH=$(dirname "$POWER_SCRIPT")

# shellcheck disable=SC1090
. "$POWER_SCRIPT_PATH/colors.sh"

readonly FONT="Fira Code 12"
readonly SEPARATOR="|"
readonly MENU="=> Lock$SEPARATOR=> Logout$SEPARATOR=> Reboot$SEPARATOR=> Shutdown"

# shellcheck disable=SC2154
readonly SELECTED="$(echo "$MENU" | rofi -no-lazy-grab \
  -width 15 \
  -lines 4 \
  -columns 1 \
  -font "$FONT" \
  -bw 0 \
  -sep $SEPARATOR \
  -location 3 \
  -padding 20 \
  -xoffset 10 \
  -yoffset 40 \
  -show-icons \
  -line-padding 10 \
  -color-window "${colors[nord0]},${colors[nord0]},${colors[nord0]}" \
  -color-normal "${colors[nord1]},${colors[nord4]},${colors[nord1]},${colors[nord9]},${colors[nord6]}" \
  -color-urgent "${colors[nord1]},${colors[nord11]},${colors[nord1]},${colors[nord11]},${colors[nord6]}" \
  -color-active "${colors[nord1]},${colors[nord9]},${colors[nord1]},${colors[nord9]},${colors[nord6]}" \
  -dmenu -i -p 'System' )"

case "$SELECTED" in
  *Lock)
    dm-tool lock
    ;;
  *Logout)
    session=$(loginctl session-status | head -n 1 | awk '{print $1}')
    loginctl terminate-session "$session"
    ;;
  *Reboot)
    systemctl reboot
    ;;
  *Shutdown)
    systemctl -i poweroff
    ;;
esac
