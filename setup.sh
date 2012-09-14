#!/bin/bash

# configuration
EXECUTE_DIR=$(cd $(dirname $0); pwd)
DOT_FILES=( zshrc oh-my-zsh gitignore emacs.el emacs.d Xresources )

# execution
for file in ${DOT_FILES[@]}
do
    ln -sf $EXECUTE_DIR/$file $HOME/.$file
done
