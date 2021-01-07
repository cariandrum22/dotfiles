#!/usr/bin/env bash

readonly COLORS_SCRIPT=$(realpath -s "$0")
readonly COLORS_SCRIPT_PATH=$(dirname "$COLORS_SCRIPT")
readonly COLORS_INI="$COLORS_SCRIPT_PATH/../colors.ini"

while IFS='= ' read -r name value
do
  if [[ $name == \#* ]]
  then
    continue
  elif [[ $name =~ \[(.*)\] ]]
  then
    section=${BASH_REMATCH[1]}
    declare -A "$section"
  elif [[ $value ]]
  then
    eval "${section}[$name]=$value"
  fi
done < "$COLORS_INI"
