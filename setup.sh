#!/bin/bash

# configuration
EXECUTE_DIR=$(cd $(dirname $0); pwd)
DOT_FILES=(
  zshrc
  zshrc.personal
  zsh_functions
  gitignore
  emacs.el
  emacs.d
  Xresources
  pythonstartup
  gemrc
  bundle
  )

# execution
for file in ${DOT_FILES[@]}
do
    ln -sf $EXECUTE_DIR/$file $HOME/.$file
done
