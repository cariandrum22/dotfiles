# Nix shell wrapper module for Elvish
#
# Provides shell continuity when entering nix-shell or nix develop,
# keeping you in Elvish instead of switching to bash.
#
# Inspired by any-nix-shell (https://github.com/haslersn/any-nix-shell)
# but implemented independently for Elvish without external dependencies.

fn nix-shell {|@args|
  for arg $args {
    if (or (eq $arg --pure) (eq $arg --command) (eq $arg --run)) {
      e:nix-shell $@args
      return
    }
  }

  var pkgs = $E:ANY_NIX_SHELL_PKGS
  var in-packages = $false
  for arg $args {
    if (eq $arg -p) {
      set in-packages = $true
    } elif (and $in-packages (not (str:has-prefix $arg "-"))) {
      set pkgs = $pkgs" "$arg
    } elif (str:has-prefix $arg "-") {
      set in-packages = $false
    }
  }

  set E:ANY_NIX_SHELL_PKGS = $pkgs
  e:nix-shell $@args --command elvish
}

fn nix {|@args|
  if (== (count $args) 0) {
    e:nix
    return
  }

  var subcommand = $args[0]

  if (and (not-eq $subcommand develop) (not-eq $subcommand run)) {
    e:nix $@args
    return
  }

  for arg $args {
    if (or (eq $arg -c) (eq $arg --command)) {
      e:nix $@args
      return
    }
  }

  var pkgs = $E:ANY_NIX_SHELL_PKGS
  for arg $args[1:] {
    if (not (str:has-prefix $arg "-")) {
      var truncated = (str:split " " $arg | take 1 | to-string)
      set pkgs = $pkgs" "$truncated
      break
    }
  }

  set E:ANY_NIX_SHELL_PKGS = $pkgs
  set E:IN_NIX_RUN = "1"
  e:nix $@args --command elvish
}

fn info {
  if (not-eq $E:IN_NIX_SHELL "") {
    var info = "nix-shell"
    if (not-eq $E:ANY_NIX_SHELL_PKGS "") {
      set info = $E:ANY_NIX_SHELL_PKGS
    }
    put $info
  } elif (not-eq $E:IN_NIX_RUN "") {
    put "nix-run"
  } else {
    put ""
  }
}
