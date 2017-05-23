#!/usr/bin/env fish

## Get absolute path of this script
set abs_path (cd (dirname status -f); pwd)

function deploy -d "Deploy files" -a "dot file list" -a "target directory"
  set -l dot_files argv[1]
  set -l tgt_dir argv[2]

  for dot_file in dot_files
    ln -sfn $abs_path/$dot_file $tgt_dir/.$dot_file
  end
end

## deploy dot files to $HOME
set dot_files \
      gitignore \
      emacs.el \
      emacs.d \
      Xresources \
      pythonstartup \
      gemrc \
      bundle \
      rubocop.yml \
      config/fish/config.fish \
      config/fish/fishfile

deploy $dot_files $HOME
