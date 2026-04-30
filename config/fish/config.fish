# Initialize
thunnus.init

# Set basic environment variables
set -x LANG en_US.UTF-8
set -x PATH /usr/local/sbin "$HOME/.local/bin" $PATH

if type -q tmux
    set -x TMUX_TMPDIR "/run/user/"(id -u)
    # Ensure DISPLAY is set for clipboard operations in tmux
    if test -n "$TMUX" -a -z "$DISPLAY"
        set -x DISPLAY :0
    end
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

function __claudius_import_hm_session_vars
    function __claudius_import_hm_session_var
        set -l name $argv[1]
        set -l value $argv[2]

        switch "$name"
            case __HM_SESS_VARS_SOURCED
                set -gx -- $name "$value"
            case CLAUDIUS_1PASSWORD_MODE
                if not set -q CLAUDIUS_1PASSWORD_MODE; and not set -q CLAUDIUS_OP_MODE
                    set -gx -- $name "$value"
                end
            case CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH
                if not set -q CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH; and not set -q CLAUDIUS_OP_SERVICE_ACCOUNT_TOKEN_PATH
                    set -gx -- $name "$value"
                end
            case CLAUDIUS_1PASSWORD_VAULT
                if not set -q CLAUDIUS_1PASSWORD_VAULT; and not set -q CLAUDIUS_OP_VAULT
                    set -gx -- $name "$value"
                end
            case '*'
                set -gx -- $name "$value"
        end
    end

    set -l hm_session_vars "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    if not test -f "$hm_session_vars"
        return
    end

    for line in (bash -lc 'unset __HM_SESS_VARS_SOURCED
source "$1" >/dev/null 2>&1
printf "__HM_SESS_VARS_SOURCED=%s\n" "$__HM_SESS_VARS_SOURCED"
printf "CLAUDIUS_1PASSWORD_MODE=%s\n" "$CLAUDIUS_1PASSWORD_MODE"
printf "CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH=%s\n" "$CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH"
printf "CLAUDIUS_1PASSWORD_VAULT=%s\n" "$CLAUDIUS_1PASSWORD_VAULT"' bash "$hm_session_vars")
        if test -n "$line"
            set -l parts (string split -m 1 '=' -- "$line")
            if test (count $parts) -eq 2
                __claudius_import_hm_session_var $parts[1] "$parts[2]"
            end
        end
    end
end

__claudius_import_hm_session_vars

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

# Home Manager owns CLAUDIUS_1PASSWORD_* defaults.
# Legacy CLAUDIUS_OP_* names are accepted only as migration fallbacks.
if not set -q CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH
    if set -q CLAUDIUS_OP_SERVICE_ACCOUNT_TOKEN_PATH
        set -gx CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH "$CLAUDIUS_OP_SERVICE_ACCOUNT_TOKEN_PATH"
    end
end
if set -q CLAUDIUS_OP_SERVICE_ACCOUNT_TOKEN_PATH
    set -e CLAUDIUS_OP_SERVICE_ACCOUNT_TOKEN_PATH
end
if not set -q CLAUDIUS_1PASSWORD_MODE
    if set -q CLAUDIUS_OP_MODE
        set -gx CLAUDIUS_1PASSWORD_MODE "$CLAUDIUS_OP_MODE"
    end
end
if set -q CLAUDIUS_OP_MODE
    set -e CLAUDIUS_OP_MODE
end
if not set -q CLAUDIUS_1PASSWORD_VAULT
    if set -q CLAUDIUS_OP_VAULT
        set -gx CLAUDIUS_1PASSWORD_VAULT "$CLAUDIUS_OP_VAULT"
    end
end
if set -q CLAUDIUS_OP_VAULT
    set -e CLAUDIUS_OP_VAULT
end

function __claudius_apply_default_op_auth
    switch "$CLAUDIUS_1PASSWORD_MODE"
        case desktop
            set -e OP_SERVICE_ACCOUNT_TOKEN OP_SESSION OP_ACCOUNT OP_CONNECT_HOST OP_CONNECT_TOKEN
        case manual
            set -e OP_SERVICE_ACCOUNT_TOKEN OP_CONNECT_HOST OP_CONNECT_TOKEN
        case service-account
            set -e OP_SESSION OP_ACCOUNT OP_CONNECT_HOST OP_CONNECT_TOKEN OP_SERVICE_ACCOUNT_TOKEN
            if not test -r "$CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH"
                echo "1Password service account token is not readable: $CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH" >&2
                return 1
            end
            set -gx OP_SERVICE_ACCOUNT_TOKEN (string trim -- (cat "$CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH"))
            if test -z "$OP_SERVICE_ACCOUNT_TOKEN"
                echo "1Password service account token file is empty: $CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH" >&2
                return 1
            end
        case '*'
            echo "Unsupported CLAUDIUS_1PASSWORD_MODE: $CLAUDIUS_1PASSWORD_MODE" >&2
            return 1
    end
end

function __claudius_current_op_vault
    if set -q CLAUDIUS_1PASSWORD_VAULT
        echo "$CLAUDIUS_1PASSWORD_VAULT"
    else
        echo Automation
    end
end

function __claudius_export_secrets
    set -l ai_vault (__claudius_current_op_vault)
    set -gx CLAUDIUS_SECRET_CF_AIG_ACCOUNT_ID "op://$ai_vault/CLOUDFLARE AI Gateway/Account ID"
    set -gx CLAUDIUS_SECRET_CF_AIG_GATEWAY_ID "op://$ai_vault/CLOUDFLARE AI Gateway/Gateway ID"
    set -gx CLAUDIUS_SECRET_CF_AIG_TOKEN "op://$ai_vault/CLOUDFLARE AI Gateway/credential"
    set -gx CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT "op://$ai_vault/Personal AI Gateway Credential/endpoint"
    set -gx CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN "op://$ai_vault/Personal AI Gateway Credential/credential"
    set -gx CLAUDIUS_SECRET_GOOGLE_CLOUD_PROJECT "op://$ai_vault/Vertex AI - personal/project"
    set -gx CLAUDIUS_SECRET_GOOGLE_CLOUD_LOCATION "op://$ai_vault/Vertex AI - personal/location"
    set -gx CLAUDIUS_SECRET_GOOGLE_APPLICATION_CREDENTIALS "op://$ai_vault/Vertex AI - personal/credential"
    set -gx CLAUDIUS_SECRET_OPENAI_API_KEY "op://$ai_vault/OpenAI Codex CLI/credential"
end

function __claudius_run_tool
    set -l tool $argv[1]
    set -e argv[1]

    __claudius_export_secrets
    command claudius secrets run -- $tool $argv
end

function op-sa --description 'Run op using the configured 1Password service account'
    set -l had_previous_mode 0
    set -l previous_mode

    if set -q CLAUDIUS_1PASSWORD_MODE
        set had_previous_mode 1
        set previous_mode "$CLAUDIUS_1PASSWORD_MODE"
    end

    set -gx CLAUDIUS_1PASSWORD_MODE service-account

    __claudius_apply_default_op_auth
    or begin
        set -l prepare_status $status
        if test $had_previous_mode -eq 1
            set -gx CLAUDIUS_1PASSWORD_MODE "$previous_mode"
        else
            set -e CLAUDIUS_1PASSWORD_MODE
        end
        return $prepare_status
    end

    command op $argv
    set -l status_code $status
    if test $had_previous_mode -eq 1
        set -gx CLAUDIUS_1PASSWORD_MODE "$previous_mode"
    else
        set -e CLAUDIUS_1PASSWORD_MODE
    end
    return $status_code
end

# Get secrets from 1Password
if type -q op; and set -q CLAUDIUS_1PASSWORD_MODE
    __claudius_apply_default_op_auth
end

if type -q op and type -q claudius
    __claudius_export_secrets

    # Base URLs
    if type -q claude
        set -x CLAUDIUS_SECRET_ANTHROPIC_BASE_URL "{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT}}/{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN}}/anthropic"
        function claude --wraps claude
            __claudius_run_tool claude $argv
        end
    end
    if type -q gemini
        set -x CLAUDIUS_SECRET_GOOGLE_VERTEX_BASE_URL "{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_ENDPOINT}}/{{$CLAUDIUS_SECRET_PERSONAL_AI_GATEWAY_TOKEN}}/google-vertex-ai"
        function gemini --wraps gemini
            __claudius_run_tool gemini $argv
        end
    end
    if type -q codex
        function codex --wraps codex
            __claudius_run_tool codex $argv
        end
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
