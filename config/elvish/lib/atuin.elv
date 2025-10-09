use math
use os
use file
use str
use re

try {
  set-env ATUIN_SESSION (atuin uuid)
} catch e {
  echo "Warning: failed to initialize ATUIN_SESSION" >&2
}
unset-env ATUIN_HISTORY_ID

fn begin-record {|line|
  try {
    if (not-eq (str:trim-space $line) "") {
      set-env ATUIN_HISTORY_ID (atuin history start -- $line)
    }
  } catch e {
    unset-env ATUIN_HISTORY_ID
  }
}

fn finish-record {|m|
  if (not (has-env ATUIN_HISTORY_ID)) {
    return
  }
  var exit-status = 0
  if (not-eq $m[error] $nil) {
    try {
      if (and (eq (kind-of $m[error]) map) (has-key $m[error] reason)) {
        if (and (eq (kind-of $m[error][reason]) map) (has-key $m[error][reason] exit-status)) {
          set exit-status = $m[error][reason][exit-status]
        } else {
          set exit-status = 127
        }
      } else {
        set exit-status = 127
      }
    } catch {
      set exit-status = 127
    }
  }
  var duration = 0
  try {
    set duration = (exact-num (math:round (* $m[duration] 1000000000)))
  } catch {
    set duration = 0
  }
  atuin history end --exit $exit-status --duration $duration -- $E:ATUIN_HISTORY_ID >/dev/null 2>&1
  unset-env ATUIN_HISTORY_ID
}

fn get-exit-status {|err|
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

fn -keymap-mode {
  try {
    if (and (has-env ATUIN_KEYMAP_MODE) (!= $E:ATUIN_KEYMAP_MODE "")) {
      put $E:ATUIN_KEYMAP_MODE
      return
    }
  } catch e {
    # Ignore and fall back to default
  }
  put emacs
}

fn -restore-env {|name value present|
  if $present {
    set-env $name $value
  } else {
    try {
      unset-env $name
    } catch e {
      # ignored
    }
  }
}

fn -run-search {|query @flags|
  var keymap = (-keymap-mode)

  var had-query = (has-env ATUIN_QUERY)
  var old-query = ""
  if $had-query {
    set old-query = $E:ATUIN_QUERY
  }
  var had-log = (has-env ATUIN_LOG)
  var old-log = ""
  if $had-log {
    set old-log = $E:ATUIN_LOG
  }
  var had-shell = (has-env ATUIN_SHELL)
  var old-shell = ""
  if $had-shell {
    set old-shell = $E:ATUIN_SHELL
  }
  var had-shell-flag = (has-env ATUIN_SHELL_ELVISH)
  var old-shell-flag = ""
  if $had-shell-flag {
    set old-shell-flag = $E:ATUIN_SHELL_ELVISH
  }

  set-env ATUIN_QUERY $query
  set-env ATUIN_LOG error
  set-env ATUIN_SHELL elvish
  set-env ATUIN_SHELL_ELVISH t

  var output = ""
  var status = 0

  # Build flag string for shell command
  var flag-str = ""
  if (> (count $flags) 0) {
    set flag-str = (str:join ' ' $flags)
  }

  var flag-block = ""
  if (not (eq $flag-str "")) {
    set flag-block = " "$flag-str
  }

  # Use bash to properly handle fd redirection (3>&1 1>&2 2>&3)
  # This swaps stdout and stderr so TUI goes to terminal and result is captured
  var cmd = "atuin search --keymap-mode="$keymap" -i"$flag-block" 3>&1 1>&2 2>&3"

  try {
    set output = (bash -c $cmd </dev/tty 2>/dev/tty | slurp)
  } catch e {
    set status = (get-exit-status $e)
    if (eq $status $nil) {
      set status = 1
    }
    set output = ""
  }

  -restore-env ATUIN_QUERY $old-query $had-query
  -restore-env ATUIN_LOG $old-log $had-log
  -restore-env ATUIN_SHELL $old-shell $had-shell
  -restore-env ATUIN_SHELL_ELVISH $old-shell-flag $had-shell-flag

  put [&status=$status &output=$output]
}

fn -clean-output {|text|
  # Simply trim all leading and trailing whitespace
  put (str:trim-space $text)
}

fn -status-text {|status|
  if (eq $status $nil) {
    put unknown
  } else {
    put $status
  }
}

fn search {|query @flags|
  var q = $query
  if (eq $q $nil) {
    set q = ""
  }
  var result = (-run-search $q $@flags)
  var status = $result[status]
  if (not (eq $status 0)) {
    put [&status=$status &text="" &execute=$false]
    return
  }

  var raw = (-clean-output $result[output])
  if (or (eq $raw $nil) (eq $raw "")) {
    put [&status=0 &text="" &execute=$false]
    return
  }

  var execute = $false
  var text = $raw
  if (str:has-prefix $raw "__atuin_accept__:") {
    set text = (str:trim-prefix "__atuin_accept__:" $raw)
    set execute = $true
  }

  put [&status=0 &text=$text &execute=$execute]
}

fn search-up {|query|
  var q = $query
  if (eq $q $nil) {
    set q = ""
  }
  search $q "--shell-up-key-binding"
}
