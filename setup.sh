#!/bin/bash

# configuration
EXECUTE_DIR=$(cd $(dirname $0); pwd)
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
  )

# execution
for file in ${DOT_FILES[@]}
do
    ln -sfn $EXECUTE_DIR/$file $HOME/.$file
done
