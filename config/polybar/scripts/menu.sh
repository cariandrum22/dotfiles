#!/usr/bin/env bash

rofi -show drun \
  -modi drun \
  -theme-str 'window {width: 450;} listview {columns: 2; lines: 15;}' \
  -location 1 \
  -xoffset 10 \
  -yoffset 40 \
  -drun-display-format "{name}" \
  -display-drun "Applications"
