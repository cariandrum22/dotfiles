# 1Password helpers for Elvish
use re
use str

fn _trim-shell-value {|raw|
  var value = (str:trim-space $raw)

  if (str:has-suffix $value ";") {
    set value = (str:trim-suffix $value ";")
  }

  var double-quoted = [(re:find '^"(.*)"$' $value)]
  if (> (count $double-quoted) 0) {
    put $double-quoted[0][groups][1][text]
    return
  }

  var single-quoted = [(re:find "^'(.*)'$" $value)]
  if (> (count $single-quoted) 0) {
    put $single-quoted[0][groups][1][text]
    return
  }

  put $value
}

fn _account-from-args {|@args|
  var wants-account = $false

  for arg $args {
    if $wants-account {
      put $arg
      return
    }

    if (eq $arg --account) {
      set wants-account = $true
      continue
    }

    var inline-account = [(re:find "^--account=(.+)$" $arg)]
    if (> (count $inline-account) 0) {
      put $inline-account[0][groups][1][text]
      return
    }
  }
}

fn _unset-env-if-set {|name|
  if (has-env $name) {
    unset-env $name
  }
}

fn _session-vars {
  var names = (bash --noprofile --norc -c 'printf "%s\n" "${!OP_SESSION@}"' | slurp)
  each {|name|
    if (re:match '^OP_SESSION(?:_[A-Za-z0-9_]+)?$' $name) {
      put $name
    }
  } [(str:split "\n" $names)]
}

fn _unset-sessions {
  each $unset-env~ [(_session-vars)]
}

fn _parse-signin-output {|output account|
  var matched = $false

  each {|line|
    if $matched {
      continue
    }

    var trimmed = (str:trim-space $line)
    if (or (eq $trimmed "") (str:has-prefix $trimmed "#")) {
      continue
    }

    var pieces = [(str:split " #" $trimmed)]
    var no-comment = (str:trim-space $pieces[0])
    var match = [(re:find "^export\\s+(OP_SESSION(?:_[A-Za-z0-9_]+)?)=(.*)$" $no-comment)]
    if (== (count $match) 0) {
      continue
    }

    var name = $match[0][groups][1][text]
    var token = (_trim-shell-value $match[0][groups][2][text])
    if (eq $token "") {
      fail "op signin returned an empty session token"
    }

    set-env $name $token
    echo (styled "✓ set " green) $name
    set matched = $true
  } [(str:split "\n" $output)]

  if (not $matched) {
    echo (styled "⚠ no OP_SESSION export found in op signin output" yellow) >&2
    return
  }

  if (not-eq $account "") {
    set-env OP_ACCOUNT $account
    echo (styled "✓ set " green) OP_ACCOUNT
  }
}

fn use-desktop-integration {
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
  _unset-sessions
  _unset-env-if-set OP_ACCOUNT
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN
}

fn use-manual-signin {
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN

  for name [(_session-vars)] {
    if (not-eq (get-env $name) "") {
      return
    }
  }
  fail "1Password manual session is not set. Run op-signin first."
}

fn _read-service-account-token {|token-path|
  if (eq $token-path "") {
    fail "1Password service account token path is empty."
  }

  if (not ?(test -r $token-path)) {
    fail "1Password service account token is not readable: "$token-path
  }

  var token = (str:trim-space (slurp < $token-path))
  if (eq $token "") {
    fail "1Password service account token file is empty: "$token-path
  }

  put $token
}

fn use-service-account {|token-path|
  _unset-sessions
  _unset-env-if-set OP_ACCOUNT
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN

  var token = [(_read-service-account-token $token-path)][0]
  set-env OP_SERVICE_ACCOUNT_TOKEN $token
  echo (styled "✓ set " green) OP_SERVICE_ACCOUNT_TOKEN
}

fn apply-shell-mode {|mode token-path|
  if (eq $mode "desktop") {
    use-desktop-integration
    return
  }

  if (eq $mode "manual") {
    _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
    _unset-env-if-set OP_CONNECT_HOST
    _unset-env-if-set OP_CONNECT_TOKEN
    return
  }

  if (eq $mode "service-account") {
    _unset-sessions
    _unset-env-if-set OP_ACCOUNT
    _unset-env-if-set OP_CONNECT_HOST
    _unset-env-if-set OP_CONNECT_TOKEN
    _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN

    var token = [(_read-service-account-token $token-path)][0]
    set-env OP_SERVICE_ACCOUNT_TOKEN $token
    return
  }

  fail "Unsupported 1Password mode: "$mode
}

fn signin {|@args|
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN

  var account = ""
  var parsed-account = [(_account-from-args $@args)]
  if (> (count $parsed-account) 0) {
    set account = $parsed-account[0]
  } elif (not-eq $E:OP_ACCOUNT "") {
    set account = $E:OP_ACCOUNT
  }

  var output = (e:op signin $@args | slurp)
  _parse-signin-output $output $account
}
