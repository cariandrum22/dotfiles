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

# Aliases
alias where="command -v"
alias j="jobs -l"
alias ll="ls -lh"
alias la="ls -lha"
alias code="code-insiders"

# Normalize path(trim tail slash)
thunnus.path.normalize $PATH
