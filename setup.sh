#!/bin/bash

## Get absolute path of this script
ABS_PATH=$(cd $(dirname $0); pwd)
DOT_FILES=(
  zshrc
  zshrc.personal
  zlogin
  zsh_functions
  gitignore
  emacs.el
  emacs.d
  Xresources
  pythonstartup
  gemrc
  bundle
  rubocop.yml
  config/fish/config.fish
  config/fish/fishfile
  config/iTerm2/com.googlecode.iterm2.plist
)

## deploy dot files to $HOME
for dot_file in ${DOT_FILES[@]}
do
    ln -sfn $ABS_PATH/$dot_file $HOME/.$dot_file
done
