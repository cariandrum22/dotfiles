# 1Password helpers for Elvish
use re
use str

fn _print-signin-error {|output|
  echo (styled "op signin failed" red) >&2
  if (not-eq (str:trim-space $output) "") {
    echo $output >&2
  }
}

fn _run-op-capture {|@args|
  var marker = "__ELVISH_OP_EXIT_STATUS__="
  var script = '
op "$@" 2>&1
status=$?
printf "\n__ELVISH_OP_EXIT_STATUS__=%s\n" "$status"
'
  var output = (sh -c $script sh $@args | slurp)

  var parts = [(str:split $marker $output)]
  if (not (== (count $parts) 2)) {
    put "1"
    put $output
    return
  }

  put (str:trim-space $parts[1])
  put (str:trim-suffix $parts[0] "\n")
}

fn _trim-shell-value {|raw|
  var value = (str:trim-space $raw)

  if (str:has-suffix $value ";") {
    set value = (str:trim-suffix $value ";")
  }

  var double-quoted = (re:find '^"(.*)"$' $value)
  if (not (eq $double-quoted $nil)) {
    put $double-quoted[groups][1][text]
    return
  }

  var single-quoted = (re:find "^'(.*)'$" $value)
  if (not (eq $single-quoted $nil)) {
    put $single-quoted[groups][1][text]
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

    var inline-account = (re:find "^--account=(.+)$" $arg)
    if (not (eq $inline-account $nil)) {
      put $inline-account[groups][1][text]
      return
    }
  }
}

fn _unset-env-if-set {|name|
  if (has-env $name) {
    unset-env $name
  }
}

fn _set-session {|session account|
  set-env OP_SESSION $session
  echo (styled "✓ set " green) OP_SESSION

  if (not (eq $account "")) {
    set-env OP_ACCOUNT $account
    echo (styled "✓ set " green) OP_ACCOUNT
  }
}

fn _set-legacy-session {|session-var raw account|
  var session-value = [(_trim-shell-value $raw)][0]

  set-env $session-var $session-value
  echo (styled "✓ set " green) $session-var

  if (not (eq $session-var OP_SESSION)) {
    set-env OP_SESSION $session-value
    echo (styled "✓ set " green) OP_SESSION
  }

  if (not (eq $account "")) {
    set-env OP_ACCOUNT $account
    echo (styled "✓ set " green) OP_ACCOUNT
  }
}

fn _parse-legacy-signin-output {|output account|
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
    var match = (re:find "^export\\s+(OP_SESSION(?:_[A-Za-z0-9_]+)?)=(.*)$" $no-comment)
    if (eq $match $nil) {
      continue
    }

    _set-legacy-session $match[groups][1][text] $match[groups][2][text] $account
    set matched = $true
  } [(str:split "\n" $output)]

  if (not $matched) {
    echo (styled "⚠ no OP_SESSION export found in op signin output" yellow) >&2
    echo $output >&2
  }
}

fn use-desktop-integration {
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
  _unset-env-if-set OP_SESSION
  _unset-env-if-set OP_ACCOUNT
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN
}

fn use-manual-signin {
  _unset-env-if-set OP_SERVICE_ACCOUNT_TOKEN
  _unset-env-if-set OP_CONNECT_HOST
  _unset-env-if-set OP_CONNECT_TOKEN

  if (eq $E:OP_SESSION "") {
    fail "1Password manual session is not set. Run op-signin first."
  }
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
  _unset-env-if-set OP_SESSION
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
    _unset-env-if-set OP_SESSION
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

  var raw-signin = [(_run-op-capture signin --raw $@args)]
  var raw-status = $raw-signin[0]
  var raw-output = $raw-signin[1]

  if (not-eq $raw-status "0") {
    var legacy-raw-unsupported = (not (eq (re:find "unknown flag:?\\s+--raw" $raw-output) $nil))

    if $legacy-raw-unsupported {
      var legacy-signin = [(_run-op-capture signin $@args)]
      if (not-eq $legacy-signin[0] "0") {
        _print-signin-error $legacy-signin[1]
        return
      }

      _parse-legacy-signin-output $legacy-signin[1] $account
      return
    }

    _print-signin-error $raw-output
    return
  }

  var session = (str:trim-space $raw-output)
  if (eq $session "") {
    echo (styled "⚠ op signin returned an empty session token" yellow) >&2
    echo $raw-output >&2
    return
  }

  _set-session $session $account
}
