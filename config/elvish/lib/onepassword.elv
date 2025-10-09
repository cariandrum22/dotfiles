# 1Password helpers for Elvish
use re
use str

fn signin {|@args|
  var output = ""
  try {
    set output = (op signin $@args 2>&1 | slurp)
  } catch e {
    echo (styled "op signin failed" red) >&2
    if (has-key $e reason) {
      echo $e[reason] >&2
    }
    if (has-key $e stderr) {
      echo $e[stderr] >&2
    }
    return
  }

  var session-var = ""
  var session-value = ""

  each {|line|
    if (not (eq $session-var "")) {
      continue
    }

    var trimmed = (str:trim-space $line)
    if (or (eq $trimmed "") (str:has-prefix $trimmed "#")) {
      continue
    }

    var pieces = [(str:split " #" $trimmed)]
    var no-comment = (str:trim-space $pieces[0])
    var match = (re:find "^export\\s+(OP_SESSION_[A-Za-z0-9_]+)=(.*)$" $no-comment)
    if (eq $match $nil) {
      continue
    }

    var name = (str:trim-space $match[groups][1][text])
    var raw = (str:trim-space $match[groups][2][text])

    if (str:has-suffix $raw ";") {
      set raw = (str:trim-suffix ";" $raw)
    }

    var value = $raw
    var double-quoted = (re:find '^"(.*)"$' $raw)
    if (not (eq $double-quoted $nil)) {
      set value = $double-quoted[groups][1][text]
    } else {
      var single-quoted = (re:find "^'(.*)'$" $raw)
      if (not (eq $single-quoted $nil)) {
        set value = $single-quoted[groups][1][text]
      }
    }

    set session-var = $name
    set session-value = $value
    set-env $session-var $session-value
    echo (styled "✓ set " green) $session-var
  } [(str:split "\n" $output)]

  if (eq $session-var "") {
    echo (styled "⚠ no OP_SESSION_* export found in op signin output" yellow) >&2
    echo $output >&2
    return
  }
}
