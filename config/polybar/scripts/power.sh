#!/usr/bin/env bash

SEPARATOR="|"
readonly SEPARATOR
MENU="=> Lock$SEPARATOR=> Logout$SEPARATOR=> Reboot$SEPARATOR=> Shutdown"
readonly MENU

# shellcheck disable=SC2154
SELECTED="$(echo "$MENU" | rofi \
  -theme-str 'window {width: 400;} listview {lines: 4;}' \
  -sep $SEPARATOR \
  -location 3 \
  -xoffset 10 \
  -yoffset 40 \
  -dmenu -i -p 'System' )"
readonly SELECTED

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
