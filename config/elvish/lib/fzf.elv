# fzf integration for Elvish inspired by PatrickF1/fzf.fish
# Provides discoverability helpers for history, files, directories, git, and processes.

use str

fn -get-exit-status {|err|
  if (not (and (eq (kind-of $err) map) (has-key $err reason))) {
    put $nil
    return
  }
  var reason = $err[reason]
  if (not (and (eq (kind-of $reason) map) (has-key $reason exit-status))) {
    put $nil
    return
  }
  put $reason[exit-status]
}

fn -run-fzf {|@extra|
  if (not (has-external fzf)) {
    echo "fzf: command not found" >&2
    put ""
    return
  }
  var defaults = []
  if (and (not (has-env FZF_DEFAULT_OPTS)) (not (has-env FZF_DEFAULT_OPTS_FILE))) {
    set defaults = ["--cycle" "--layout=reverse" "--border" "--height=90%" "--preview-window=wrap"]
  }
  fzf $@defaults $@extra
}

fn -warn {|msg|
  echo $msg >&2
}

fn -status-text {|status|
  if (eq $status $nil) {
    put "unknown"
  } else {
    put $status
  }
}

fn history-search {
  var query = ""
  try {
    set query = (edit:current-command)
  } catch e {
    # edit module not available yet
  }

  var result = ""
  try {
    set result = (edit:command-history &dedup &cmd-only |
      to-lines |
      -run-fzf --prompt="History> " --reverse --query=$query)
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: history search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }
  try {
    edit:replace-input $result
  } catch e {
    echo $result
  }
}

fn file-search {
  var finder = ""
  if (has-external fdfind) {
    set finder = "fdfind"
  } elif (has-external fd) {
    set finder = "fd"
  }

  var result = ""
  try {
    if (eq $finder "") {
      set result = (find . -type f 2>/dev/null |
        -run-fzf --prompt="Files> " --height=70% --reverse --preview="head -n 200 {}")
    } else {
      set result = ((e:$finder) --color=always --hidden --follow --type f . |
        -run-fzf --ansi --prompt="Files> " --height=70% --reverse --preview="head -n 200 {}")
    }
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: file search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }
  try {
    edit:insert-at-dot $result
  } catch e {
    echo $result
  }
}

fn dir-search {
  var finder = ""
  if (has-external fdfind) {
    set finder = "fdfind"
  } elif (has-external fd) {
    set finder = "fd"
  }

  var result = ""
  try {
    if (eq $finder "") {
      set result = (find . -type d 2>/dev/null |
        -run-fzf --prompt="Directories> " --height=70% --reverse --preview="ls {}")
    } else {
      set result = ((e:$finder) --color=always --hidden --follow --type d . |
        -run-fzf --ansi --prompt="Directories> " --height=70% --reverse --preview="ls {}")
    }
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: directory search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }

  try {
    cd $result
    edit:redraw &full=$true
  } catch e {
    -warn (str:fmt "fzf: failed to cd into %s" $result)
  }
}

fn git-log-search {
  if (not (has-external git)) {
    -warn "fzf: git not available"
    return
  }

  var result = ""
  try {
    set result = (git --no-pager log --decorate --color=always --pretty=format:"%h\t%C(auto)%s %C(blue)%cr %C(auto)%d" |
      -run-fzf --ansi --prompt="Git Log> " --height=80% --reverse --preview="git --no-pager show --color=always {1}" --preview-window="down,60%")
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: git log search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }
  var parts = [(str:split "\t" $result)]
  if (eq (count $parts) 0) {
    return
  }
  var commit = $parts[0]
  try {
    edit:replace-input $commit
  } catch e {
    echo $commit
  }
}

fn process-search {
  if (not (has-external ps)) {
    -warn "fzf: ps not available"
    return
  }

  var result = ""
  try {
    set result = (ps -eo pid,comm --sort=-%mem |
      -run-fzf --prompt="Processes> " --height=70% --reverse --header-lines=1 --preview="ps -p {1} -o pid,ppid,%cpu,%mem,etime,cmd")
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: process search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }
  var parts = [(str:split " " (str:trim-space $result))]
  if (eq (count $parts) 0) {
    return
  }
  var pid = $parts[0]
  try {
    edit:insert-at-dot $pid
  } catch e {
    echo $pid
  }
}

fn variable-search {
  var result = ""
  try {
    set result = (env |
      -run-fzf --prompt="Variables> " --height=70% --reverse --preview="echo {}")
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: variable search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }
  var parts = [(str:split "=" $result)]
  if (eq (count $parts) 0) {
    return
  }
  var name = $parts[0]
  try {
    edit:insert-at-dot $name
  } catch e {
    echo $name
  }
}

fn ghq-search {
  if (not (has-external ghq)) {
    return
  }
  var result = ""
  try {
    set result = (ghq list --full-path |
      -run-fzf --prompt="GHQ> " --height=70% --reverse --preview="ls {}")
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: ghq search failed (exit %s)" (-status-text $status))
    return
  }
  if (eq $result "") {
    return
  }
  try {
    cd $result
    edit:redraw &full=$true
  } catch e {
    -warn (str:fmt "fzf: failed to cd into %s" $result)
  }
}

fn tmux-session-search {
  if (not (has-external tmux)) {
    -warn "fzf: tmux not available"
    return
  }

  # Check if there are any sessions
  var session-count = 0
  try {
    set session-count = (tmux list-sessions 2>/dev/null | count)
  } catch e {
    -warn "fzf: no tmux sessions found"
    return
  }

  if (eq $session-count 0) {
    -warn "fzf: no tmux sessions found"
    return
  }

  var result = ""
  try {
    set result = (tmux list-sessions -F "#{session_name}#{?session_attached, (attached),}: #{session_windows} windows (created #{session_created_string})" 2>/dev/null |
      -run-fzf --prompt="Tmux Sessions> " --height=70% --reverse --preview="tmux list-windows -t {1}")
  } catch e {
    var status = (-get-exit-status $e)
    if (or (eq $status 1) (eq $status 130)) {
      return
    }
    -warn (str:fmt "fzf: tmux session search failed (exit %s)" (-status-text $status))
    return
  }

  if (eq $result "") {
    return
  }

  # Extract session name (before the colon or space)
  var parts = [(str:split ":" $result)]
  if (eq (count $parts) 0) {
    return
  }
  var session = (str:trim-space $parts[0])

  # Check if we're inside tmux
  try {
    if (has-env TMUX) {
      # Inside tmux: switch to the session
      tmux switch-client -t $session
    } else {
      # Outside tmux: attach to the session
      tmux attach-session -t $session
    }
  } catch e {
    -warn (str:fmt "fzf: failed to switch to tmux session %s" $session)
  }
}

fn -apply-binding {|key fn|
  if (or (eq $key $nil) (eq $key "")) {
    return
  }
  try {
    set edit:insert:binding[$key] = $fn
  } catch e {
    # edit module not yet available
  }
}

fn configure-bindings {|&history="Ctrl-R" &files="Ctrl-T" &directory="Alt-C" &git_log="Alt-L" &processes="Alt-P" &variables="Alt-V" &ghq="Ctrl-G" &tmux="Alt-T"|
  -apply-binding $history $history-search~
  -apply-binding $files $file-search~
  -apply-binding $directory $dir-search~
  -apply-binding $git_log $git-log-search~
  -apply-binding $processes $process-search~
  -apply-binding $variables $variable-search~
  -apply-binding $ghq $ghq-search~
  -apply-binding $tmux $tmux-session-search~
}

fn setup {
  configure-bindings
}
