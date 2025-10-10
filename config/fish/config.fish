# Initialize
thunnus.init

# Set basic environment variables
set -x LANG en_US.UTF-8
set -x PATH /usr/local/sbin "$HOME/.local/bin" $PATH

if type -q tmux
    set -x TMUX_TMPDIR "/run/user/"(id -u)
end

if type -q emacs
    set -x EDITOR (which emacs)
end

if type -q code
    set -x VISUAL (which code)
end

# Configure Nix
if not type -q nixos-version
    set -x NIX_PATH "$HOME/.nix-defexpr/channels"
end

if type -q bass
    if [ -f "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]
        bass source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
    end
end

# Configure any-nix-shell
if type -q any-nix-shell
    any-nix-shell fish --info-right | source
end

# Configure XDG Base Directory
set -x XDG_CONFIG_HOME "$HOME/.config"

# Configure Homebrew
if [ -f /opt/homebrew/bin/brew ]
    eval $(/opt/homebrew/bin/brew shellenv)
end

# Configure Go
if [ -d "$HOME/Go" ]
    set -x GOPATH "$HOME/Go"
    set_path "$GOPATH/bin"
end

# Configure kitty
if type -q kitty
    alias ssh="TERM=xterm-256color $(which ssh)"
end

# Configure fzf
if type -q fzf
    set -U FZF_LEGACY_KEYBINDINGS 0
end

# Configure atuin (shell history in SQLite)
if type -q atuin
    atuin init fish | source
end

# Configure direnv
if type -q direnv
    eval (direnv hook fish)
end

# Configure Google Cloud SDK
if [ -f $HOME/.nix-profile/google-cloud-sdk/path.fish.inc ]
    source "$HOME/.nix-profile/google-cloud-sdk/path.fish.inc"
end

# Configure rustup
if type -q rustup
    set_path "$HOME/.cargo/bin"
    rustup completions fish | source
end

# Configure Java
if type -q java
    if [ (uname) = Darwin ]
        set -x JAVA_HOME (/usr/libexec/java_home)
    else
        set -x JAVA_HOME (type -p javac|xargs readlink -f|xargs dirname|xargs dirname)
    end
end

# Configure JavaScript
if type -q node
    switch (uname)
        case Darwin
            set total_mem (sysctl -n hw.memsize)
        case Linux
            set total_mem (math (awk '/MemTotal/ {print $2}' /proc/meminfo) \* 1024)
        case '*'
            # exit from if block only
    end

    if set -q total_mem
        set -x NODE_OPTIONS "--max-old-space-size=$(math round $total_mem / 4)"
    end
end

# Configure Python
if type -q python
    set -x PYTHONSTARTUP "$HOME/.pythonstartup"
end

# Configure Texinfo
if [ (uname) = Darwin ]
    if [ -d /usr/local/opt/texinfo/bin ]
        set_path /usr/local/opt/texinfo/bin
    end
end

# For WSL
if [ -f /proc/sys/fs/binfmt_misc/WSLInterop ]
    set -x PATH "/mnt/c/Program Files/Microsoft VS Code/bin" $PATH
    thunnus.gpg.agent_relay >/dev/null
end

# For Terragrunt with OpenTofu
set -x TG_TF_PATH "$HOME/.local/bin/tofu"

# Configure Krew
if type -q krew
    set_path "$HOME/.krew/bin"
end

# For Gemini-CLI
if type -q op and type -q gemini
    set -x GOOGLE_CLOUD_PROJECT "op://Private/Vertex AI - personal/project"
    set -x GOOGLE_CLOUD_LOCATION "op://Private/Vertex AI - personal/location"
    set -x GOOGLE_APPLICATION_CREDENTIALS "op://Private/Vertex AI - personal/credential"
end

# Get secrets from 1Password
if type -q op and type -q claudius
    set -x CLAUDIUS_SECRET_CF_AIG_ACCOUNT_ID "op://Private/CLOUDFLARE AI Gateway/Account ID"
    set -x CLAUDIUS_SECRET_CF_AIG_GATEWAY_ID "op://Private/CLOUDFLARE AI Gateway/Gateway ID"
    set -x CLAUDIUS_SECRET_CF_AIG_TOKEN "op://Private/CLOUDFLARE AI Gateway/credential"
    set -x CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT "op://Private/Personal AI Gateway Credential/endpoint"
    set -x CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN "op://Private/Personal AI Gateway Credential/credential"

    # Base URLs
    if type -q claude
        set -x CLAUDIUS_SECRET_ANTHROPIC_BASE_URL "{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT}}/{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN}}/anthropic"
        alias claude="claudius run -- claude"
    end
    if type -q gemini
        set -x CLAUDIUS_SECRET_GOOGLE_VERTEX_BASE_URL "{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT}}/{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN}}/google-vertex-ai"
        alias gemini="claudius run -- gemini"
    end
    if type -q codex
        set -x CLAUDIUS_SECRET_OPENAI_API_KEY "op://Private/OpenAI Codex CLI/credential"
        set -x CLAUDIUS_SECRET_OPENAI_BASE_URL "{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT}}/{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN}}/openai"
        alias codex="claudius run -- codex"
    end
end

# Aliases
alias where="command -v"
alias j="jobs -l"
alias ll="ls -lh"
alias la="ls -lha"
alias code="code-insiders"

# Normalize path(trim tail slash)
thunnus.path.normalize $PATH
