# Elvish configuration - Mathematical symbol theme
# Managed by Home Manager

use str
use nix-shell
use fzf
use rivendell/base b
use rivendell/fun f

# Setup fzf key bindings
fzf:setup

# Use GNU readline-style key bindings (like Bash)
# This provides Ctrl-A (line start), Ctrl-E (line end), Ctrl-B/F (move char),
# Alt-B/F (move word), and other familiar Emacs-style bindings
use readline-binding

fn _seg {|label val color|
  if (and (not-eq $val "") (not-eq $val $nil)) {
    put (styled $label dim)" "(styled "⟦" dim)(styled $val $color)(styled "⟧" dim)
  }
}

fn _git_branch {
  try {
    git rev-parse --abbrev-ref HEAD 2>/dev/null
  } catch {
    put ""
  }
}

fn _git_status_all {
  try {
    var s = (git status --porcelain 2>/dev/null)
    if (eq $s "") {
      put [&added=0 &modified=0 &untracked=0]
      return
    }
    # Functional approach: reduce over lines to accumulate status counts
    var lines = [(splits "\n" $s)]
    # Prepend initial state to lines for reduce (rivendell's reduce uses first element as initial)
    var with_initial = [[&added=0 &modified=0 &untracked=0] $@lines]
    var result = (f:reduce {|acc line|
      # Skip if line is the initial map (first iteration)
      if (eq (kind-of $line) map) {
        put $line
      } else {
        var a = $acc[added]
        var m = $acc[modified]
        var u = $acc[untracked]
        if (str:has-prefix $line "A ") { set a = (b:inc $a) }
        if (or (str:has-prefix $line " M") (str:has-prefix $line "M ")) { set m = (b:inc $m) }
        if (str:has-prefix $line "??") { set u = (b:inc $u) }
        put [&added=$a &modified=$m &untracked=$u]
      }
    } $with_initial)
    put $result
  } catch {
    put [&added=0 &modified=0 &untracked=0]
  }
}

fn _is_ssh {
  and (has-env SSH_TTY) (not-eq $E:SSH_TTY "")
}

fn _python_venv {
  if (and (has-env VIRTUAL_ENV) (not-eq $E:VIRTUAL_ENV "")) {
    put (path:base $E:VIRTUAL_ENV)
  } else {
    put ""
  }
}

# Generic version extractor using functional composition
fn _extract_version {|cmd|
  try {
    var ver = (e:$cmd --version 2>&1)
    var parts = [(echo $ver | awk '{print $2}' | str:split "." (all))]
    if (>= (count $parts) 2) {
      put (b:first $parts)"."(b:second $parts)
    } else {
      put ""
    }
  } catch {
    put ""
  }
}

# Create partially applied version checkers
var _get_python_ver = (f:partial $_extract_version~ python3)
var _get_rust_ver = (f:partial $_extract_version~ rustc)

fn _python_version {
  if (has-external python3) {
    $_get_python_ver
  } else {
    put ""
  }
}

fn _rust_version {
  if (and ?(test -f Cargo.toml) (has-external rustc)) {
    $_get_rust_ver
  } else {
    put ""
  }
}

fn _aws_info {
  # Functional approach: collect non-empty values and join
  var profile = ""
  var region = ""
  if (has-env AWS_PROFILE) { set profile = $E:AWS_PROFILE }
  if (has-env AWS_REGION) { set region = $E:AWS_REGION }

  var parts = [(each {|x| if (not-eq $x "") { put $x }} [$profile $region])]
  if (b:is-empty $parts) {
    put ""
  } else {
    put (str:join ":" $parts)
  }
}

fn _k8s_context {
  if (has-external kubectl) {
    try {
      var ctx = (kubectl config current-context 2>/dev/null)
      # Extract cluster name from ARN using functional approach
      if (str:contains $ctx "cluster/") {
        var parts = [(str:split "/" $ctx)]
        put (b:end $parts)  # Use rivendell's end function
      } else {
        put $ctx
      }
    } catch {
      put ""
    }
  } else {
    put ""
  }
}

fn _background_jobs {
  var count = 0
  try {
    set count = (jobs | count)
  } catch {
    set count = 0
  }
  if (> $count 0) {
    put (to-string $count)
  } else {
    put ""
  }
}

var -last-duration = 0

set edit:after-command = [
  $@edit:after-command
  {|m| set -last-duration = $m[duration] }
]

set edit:before-readline = [
  $@edit:before-readline
  { }
]

set edit:prompt = {
  # Line 1: Context (host, path, git, exit status)
  var cwd = (tilde-abbr $pwd)
  var br = (_git_branch)
  var st = (_git_status_all)
  var nix-info = (nix-shell:info)

  if (_is_ssh) {
    _seg "◉" (hostname) cyan
    styled " ⟂ " dim
  }

  if (not-eq $nix-info "") {
    _seg "❄" $nix-info bright-blue
    styled " ⟂ " dim
  }

  _seg "∈" $cwd bright-cyan

  if (not-eq $br "") {
    styled " ⟂ " dim
    _seg "⊂" $br bright-blue
  }

  # Δ (delta) = added, ∇ (nabla) = modified, ∃ (exists) = untracked
  if (> $st[added] 0) {
    styled " " dim
    _seg "Δ" "+"(to-string $st[added]) magenta
  }

  if (> $st[modified] 0) {
    styled " " dim
    _seg "∇" "~"(to-string $st[modified]) yellow
  }

  if (> $st[untracked] 0) {
    styled " " dim
    _seg "∃" "?"(to-string $st[untracked]) blue
  }

  put "\n"

  # Line 2: Environment info (venv, versions, AWS, K8s, jobs)
  # Functional approach: define info descriptors and filter/map over them
  var env_infos = [
    [&label="λ" &value=(_python_venv) &color=yellow]
    [&label="🐍" &value=(_python_version) &color=green]
    [&label="🦀" &value=(_rust_version) &color=red]
    [&label="☁" &value=(_aws_info) &color=yellow]
    [&label="⎈" &value=(_k8s_context) &color=blue]
    [&label="⚙" &value=(_background_jobs) &color=cyan]
  ]

  # Filter out empty values and render segments
  var active_infos = [(each {|info| if (not-eq $info[value] "") { put $info }} $env_infos)]

  if (not (b:is-empty $active_infos)) {
    var first = $true
    each {|info|
      if (not $first) {
        styled " · " dim
      } else {
        set first = $false
      }
      _seg $info[label] $info[value] $info[color]
    } $active_infos
    put "\n"
  }

  # Line 3: Input prompt
  styled "⊢ " bold
}

set edit:rprompt = {
  var ts = (date +%Y-%m-%dT%H:%M:%S)
  var dt-ms = (printf "%.0f" (* $-last-duration 1000))
  var dts = (printf "%03.0f" $dt-ms)
  styled "⧖ "$ts"  ─  Δt "$dts" ms" inverse
}

# Basic environment variables
set E:LANG = en_US.UTF-8

# Set EDITOR and VISUAL if available
if (has-external emacs) {
  set E:EDITOR = (which emacs)
}
if (has-external code-insiders) {
  set E:VISUAL = (which code-insiders)
}

# Add paths to PATH
set paths = [
  $E:HOME/.local/bin
  /usr/local/sbin
  $@paths
]

# Configure tmux
if (has-external tmux) {
  set E:TMUX_TMPDIR = "/run/user/"(id -u)
}

# Configure Nix
if (not (has-external nixos-version)) {
  set E:NIX_PATH = $E:HOME/.nix-defexpr/channels
}

# Source Nix daemon profile
if ?(test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh) {
  # Add Nix paths
  set paths = [
    /nix/var/nix/profiles/default/bin
    $@paths
  ]
}

# Configure XDG Base Directory
set E:XDG_CONFIG_HOME = $E:HOME/.config

# Configure Homebrew (macOS)
if ?(test -f /opt/homebrew/bin/brew) {
  set paths = [
    /opt/homebrew/bin
    /opt/homebrew/sbin
    $@paths
  ]
  set E:HOMEBREW_PREFIX = /opt/homebrew
  set E:HOMEBREW_CELLAR = /opt/homebrew/Cellar
  set E:HOMEBREW_REPOSITORY = /opt/homebrew
}

# Configure Go
if ?(test -d $E:HOME/Go) {
  set E:GOPATH = $E:HOME/Go
  set paths = [
    $E:GOPATH/bin
    $@paths
  ]
}

# Configure direnv (module is generated by home-manager)
if (has-external direnv) {
  use direnv
}

# Configure rustup
if (has-external rustup) {
  set paths = [
    $E:HOME/.cargo/bin
    $@paths
  ]
}

# Configure Java
if (has-external java) {
  if (eq (uname) Darwin) {
    set E:JAVA_HOME = (/usr/libexec/java_home)
  } else {
    # Linux: find Java home from javac
    if (has-external javac) {
      try {
        set E:JAVA_HOME = (dirname (dirname (readlink -f (which javac))))
      } catch {
        # Ignore errors if Java setup is incomplete
      }
    }
  }
}

# Configure JavaScript/Node.js memory settings
if (has-external node) {
  var total-mem = 0
  try {
    if (eq (uname) Darwin) {
      set total-mem = (sysctl -n hw.memsize)
    } elif (eq (uname) Linux) {
      set total-mem = (* (head -1 /proc/meminfo | awk '{print $2}') 1024)
    }

    if (!= $total-mem 0) {
      set E:NODE_OPTIONS = "--max-old-space-size="(printf %.0f (/ $total-mem 4))
    }
  } catch {
    # Ignore errors in memory calculation
  }
}

# Configure Python
if (has-external python) {
  set E:PYTHONSTARTUP = $E:HOME/.pythonstartup
}

# Configure Texinfo (macOS)
if (and (eq (uname) Darwin) ?(test -d /usr/local/opt/texinfo/bin)) {
  set paths = [
    /usr/local/opt/texinfo/bin
    $@paths
  ]
}

# For WSL
if ?(test -f /proc/sys/fs/binfmt_misc/WSLInterop) {
  set paths = [
    "/mnt/c/Program Files/Microsoft VS Code/bin"
    $@paths
  ]
}

# For Terragrunt with OpenTofu
set E:TG_TF_PATH = $E:HOME/.local/bin/tofu

# Configure Krew
if (has-external krew) {
  set paths = [
    $E:HOME/.krew/bin
    $@paths
  ]
}

# For Gemini-CLI with 1Password
if (and (has-external op) (has-external gemini)) {
  set E:GOOGLE_CLOUD_PROJECT = "op://Private/Vertex AI - personal/project"
  set E:GOOGLE_CLOUD_LOCATION = "op://Private/Vertex AI - personal/location"
  set E:GOOGLE_APPLICATION_CREDENTIALS = "op://Private/Vertex AI - personal/credential"
}

# 1Password helpers
use onepassword
fn op-signin {|@args|
  if (not (has-external op)) {
    echo (styled "1Password CLI (op) not found in PATH" red) >&2
    return
  }
  onepassword:signin $@args
}

# Get secrets from 1Password for AI tools
if (and (has-external op) (has-external claudius)) {
  set E:CLAUDIUS_SECRET_CF_AIG_ACCOUNT_ID = "op://Private/CLOUDFLARE AI Gateway/Account ID"
  set E:CLAUDIUS_SECRET_CF_AIG_GATEWAY_ID = "op://Private/CLOUDFLARE AI Gateway/Gateway ID"
  set E:CLAUDIUS_SECRET_CF_AIG_TOKEN = "op://Private/CLOUDFLARE AI Gateway/credential"
  set E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT = "op://Private/Personal AI Gateway Credential/endpoint"
  set E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN = "op://Private/Personal AI Gateway Credential/credential"

  # Base URLs for AI tools with wrapper functions
  # Note: Use edit:add-var to make functions override external commands
  # Note: Use string concatenation to expand variables inside {{}} for Claudius
  if (has-external claude) {
    set E:CLAUDIUS_SECRET_ANTHROPIC_BASE_URL = "{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT"}}/{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN"}}/anthropic"
    fn -claude-wrapper {|@args|
      e:claudius secrets run -- claude $@args
    }
    edit:add-var claude~ $-claude-wrapper~
  }
  if (has-external gemini) {
    set E:CLAUDIUS_SECRET_GOOGLE_VERTEX_BASE_URL = "{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT"}}/{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN"}}/google-vertex-ai"
    fn -gemini-wrapper {|@args|
      e:claudius secrets run -- gemini $@args
    }
    edit:add-var gemini~ $-gemini-wrapper~
  }
  if (has-external codex) {
    set E:CLAUDIUS_SECRET_OPENAI_API_KEY = "op://Private/OpenAI Codex CLI/credential"
    set E:CLAUDIUS_SECRET_OPENAI_BASE_URL = "{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT"}}/{{"$E:CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN"}}/openai"
    fn -codex-wrapper {|@args|
      e:claudius secrets run -- codex $@args
    }
    edit:add-var codex~ $-codex-wrapper~
  }
}

# Aliases (using fn in Elvish)
fn where {|@args| which $@args }
fn j {|@args| jobs $@args }
fn ll {|@args| ls -lh $@args }
fn la {|@args| ls -lha $@args }
fn code {|@args| code-insiders $@args }

# Configure kitty ssh workaround
if (has-external kitty) {
  fn ssh {|@args|
    set E:TERM = xterm-256color
    e:ssh $@args
  }
}

# Configure Atuin integration (declaratively managed via Nix)
if (has-external atuin) {
  try {
    use atuin

    fn _atuin-apply-result {|res|
      if (or (eq $res $nil) (not (eq (kind-of $res) map))) {
        return
      }
      if (not (has-key $res status)) {
        return
      }
      var status = $res[status]
      if (or (eq $status $nil) (not (eq $status 0))) {
        return
      }
      if (not (has-key $res text)) {
        return
      }
      var text = $res[text]
      if (eq $text "") {
        return
      }
      try {
        edit:replace-input $text
      } catch e {
        echo $text
      }
      try {
        edit:redraw &full=$true
      } catch e {
        # ignore
      }
    }

    fn _atuin-search {|@flags|
      var query = ""
      try {
        set query = (edit:current-command)
      } catch e {
        set query = ""
      }
      var res = (atuin:search $query $@flags)
      if (and (eq (kind-of $res) map) (has-key $res status)) {
        var status = $res[status]
        if (or (eq $status 1) (eq $status 130)) {
          return
        }
        if (not (eq $status 0)) {
          try {
            fzf:history-search
          } catch e {
            # fallback unavailable
          }
          return
        }
      }
      _atuin-apply-result $res
    }

    fn _atuin-search-up {
      var line = ""
      try {
        set line = (str:trim-space $edit:current-command)
      } catch e {
        set line = ""
      }
      if (eq $line "") {
        set line = $pwd
      }
      var res = (atuin:search-up $line)
      if (and (eq (kind-of $res) map) (has-key $res status)) {
        var status = $res[status]
        if (or (eq $status 1) (eq $status 130)) {
          return
        }
        if (not (eq $status 0)) {
          return
        }
      }
      _atuin-apply-result $res
    }

    set edit:after-readline = [$@edit:after-readline {|line| atuin:begin-record $line }]
    set edit:after-command = [$@edit:after-command {|m| atuin:finish-record $m }]

    set edit:insert:binding[Ctrl-R] = $_atuin-search~
    set edit:insert:binding[Up] = $_atuin-search-up~
  } catch e {
    echo "Warning: Failed to load atuin module, using fzf for history search" >&2
  }
}

# Configure zoxide (smarter cd command)
if (has-external zoxide) {
  eval (zoxide init elvish --hook prompt | slurp)
}

# Configure Carapace (multi-shell completion)
if (has-external carapace) {
  set-env CARAPACE_BRIDGES 'zsh,fish,bash,inshellisense'
  eval (carapace _carapace | slurp)
}
