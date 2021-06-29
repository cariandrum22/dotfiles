# Initialize
thunnus.init

# Set basic environment variables
set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8
set -x PATH "/usr/local/sbin" "$HOME/.local/bin" $PATH

if type -q emacs
  set -x EDITOR (which emacs)
end

if type -q code
  set -x VISUAL (which code)
end

if type -q gpg
  set -x GPG_TTY (tty)
  set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
  gpgconf --launch gpg-agent
end

# Load OneLogin's API Credential
if [ -f "$HOME/.onelogin/credential.fish" ]
  source "$HOME/.onelogin/credential.fish"
end

# Configure nix-shell
if type -q bass
  if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]
    bass source "$HOME/.nix-profile/etc/profile.d/nix.sh"
  end
end

# Configure any-nix-shell
if type -q any-nix-shell
  any-nix-shell fish --info-right | source
end

# If cabal exists, add the path to the binary
if type -q cabal
  set_path "$HOME/.cabal/bin"
end

# Configure GOPATH
if [ -d "$HOME/Go" ]
  set -x GOPATH "$HOME/Go"
  set_path "$GOPATH/bin"
  set_path "$GOPATH/bin/"(uname -s | awk '{print tolower($0)}')"_amd64"
end

# Configuring the compiler and linker for macOS
if [ (thunnus.os.platform.detect) = macOS ]
  if [ -f "/usr/local/opt/llvm/bin" ]
    set -x CPPFLAGS "-I/usr/local/opt/llvm/include" "-I/usr/local/opt/openssl/include"
    set -x LDFLAGS "-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib" "-L/usr/local/opt/openssl/lib"
    set_path "/usr/local/opt/llvm/bin"
  end
end

# Configure fzf
if type -q fzf
  set -U FZF_LEGACY_KEYBINDINGS 0
end

# Configure direnv
if type -q direnv
  eval (direnv hook fish)
end

# Configure asdf-vm
if [ -f "$HOME/.asdf/asdf.fish" ]
  source ~/.asdf/asdf.fish
end

# Configuration for aws cli and related tools
## Export default aws credentials
#if [ -f "$HOME/.aws/credentials" ]
#  thunnus.aws.credential_selector saml tf
#end
set -x AWS_DEFAULT_REGION "ap-northeast-1"

# Configure the Google Cloud SDK
## The next line updates PATH for the Google Cloud SDK.
if [ -f $HOME/Applications/Google/google-cloud-sdk/path.fish.inc ]
  source "$HOME/Applications/Google/google-cloud-sdk/path.fish.inc"
end
## The next line enables shell command completion for gcloud.
if [ -f $HOME/Applications/Google/google-cloud-sdk/completion.fish.inc ]
  source "$HOME/Applications/Google/google-cloud-sdk/completion.fish.inc"
end

# Configure rustup
if type -q rustup
  set_path "$HOME/.cargo/bin"
  rustup completions fish | source
end

# Configure Java
if type -q java
  if [ (thunnus.os.platform.detect) = macOS ]
    set -x JAVA_HOME (/usr/libexec/java_home)
  else
    set -x JAVA_HOME (type -p javac|xargs readlink -f|xargs dirname|xargs dirname)
  end
end

# Configure Python
if type -q python
  set -x PYTHONSTARTUP "$HOME/.pythonstartup"
end

# Configure nsible
if type -q ansible
  set -x ANSIBLE_COW_SELECTION "random"
end

# Configure Texinfo
if [ (thunnus.os.platform.detect) = macOS ]
  if [ -d "/usr/local/opt/texinfo/bin" ]
    set_path "/usr/local/opt/texinfo/bin"
  end
end

# Configure aliases
## Generic
alias where="command -v"
alias j="jobs -l"
alias ll="ls -lh"
alias la="ls -lha"

# Normalize path(trim tail slash)
thunnus.path.normalize $PATH
