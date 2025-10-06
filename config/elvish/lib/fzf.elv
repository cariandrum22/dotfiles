# fzf integration for Elvish
# Provides key bindings for fuzzy finding files, history, directories, and repositories

# Ctrl-R: Search command history with fzf
fn history-search {
  var selected = (
    edit:command-history &dedup &cmd-only |
    to-lines |
    fzf --height=40% --reverse --query=(edit:current-command)
  )
  if (not-eq $selected "") {
    edit:replace-input $selected
  }
}

# Ctrl-T: Search files in current directory with fzf
fn file-search {
  var selected = (
    find . -type f 2>/dev/null |
    fzf --height=40% --reverse --preview="head -n 10 {}"
  )
  if (not-eq $selected "") {
    edit:insert-at-dot $selected
  }
}

# Alt-C: Change directory with fzf
fn dir-search {
  var selected = (
    find . -type d 2>/dev/null |
    fzf --height=40% --reverse --preview="ls {}"
  )
  if (not-eq $selected "") {
    cd $selected
    edit:redraw &full=$true
  }
}

# Ctrl-G: Search ghq repositories with fzf
fn ghq-search {
  if (not (has-external ghq)) {
    return
  }
  var selected = (
    ghq list --full-path |
    fzf --height=40% --reverse --preview="ls {}"
  )
  if (not-eq $selected "") {
    cd $selected
    edit:redraw &full=$true
  }
}

# Setup key bindings
fn setup {
  set edit:insert:binding[Ctrl-R] = $history-search~
  set edit:insert:binding[Ctrl-T] = $file-search~
  set edit:insert:binding[Alt-C] = $dir-search~
  set edit:insert:binding[Ctrl-G] = $ghq-search~
}
