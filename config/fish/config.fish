# Initialize
thunnus.init

# Basic configuration
set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8
set -x PATH "/usr/local/sbin" $PATH
set -x EDITOR (which emacs)
set -x VISUAL (which code)

# Compiler and linker flag configuration
set -x CPPFLAGS "-I/usr/local/opt/llvm/include" "-I/usr/local/opt/openssl/include"
set -x LDFLAGS "-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib" "-L/usr/local/opt/openssl/lib"

# Export default aws credentials
thunnus.aws.credential_selector codetakt:cariandrum22 tf

# fzf
set -U FZF_LEGACY_KEYBINDINGS 0

# AWS
set -x AWS_DEFAULT_REGION "ap-northeast-1"

# Google Computing Platform
## The next line updates PATH for the Google Cloud SDK.
if [ -f $HOME/Applications/Google/google-cloud-sdk/path.fish.inc ]
  source "$HOME/Applications/Google/google-cloud-sdk/path.fish.inc"
end

## The next line enables shell command completion for gcloud.
if [ -f $HOME/Applications/Google/google-cloud-sdk/completion.fish.inc ]
  source "$HOME/Applications/Google/google-cloud-sdk/completion.fish.inc"
end

# Initialize the *env such as rbenv, ndenv, etc.
arbenv "rbenv" "nodenv" "goenv" "pyenv" "plenv"

# direnv
eval (direnv hook fish)

# pyenv-virtualenv
pyenv virtualenv-init - | source

# LLVM
set_path "/usr/local/opt/llvm/bin"

# Go
set -x GOENV_ROOT "$HOME/.goenv"
set -x GOPATH "$HOME/Go"
set_path "$GOENV_ROOT/bin"
set_path "$GOPATH/bin"
goenv init - | source

# Stack
set_path "$HOME/.local/bin"

# rustup
set_path "$HOME/.cargo/bin"
rustup completions fish | source

# java
if [ (thunnus.os.platform.detect) = macOS ]
  set -x JAVA_HOME (/usr/libexec/java_home)
else
  set -x JAVA_HOME (type -p javac|xargs readlink -f|xargs dirname|xargs dirname)
end

# python
set -x PYTHONSTARTUP "$HOME/.pythonstartup"

# Terraform
set_path "$HOME/Applications/HashiCorp/Terraform/current/bin"

# Ansible
set -x ANSIBLE_COW_SELECTION "random"

# Texinfo
set_path "/usr/local/opt/texinfo/bin"

# Nand2tetris Software Suite
set_path "$HOME/Applications/nand2tetris/tools"

# CoreOS tools
set_path "$HOME/Applications/CoreOS/bin"

# alias
## Generic
alias where="command -v"
alias j="jobs -l"
alias ll="ls -lh"
alias la="ls -lha"

## Haskell
#. $HOME/.nix-profile/etc/profile.d/nix.sh
alias ghc="stack ghc --"
alias ghci="stack ghci"
alias runghc="stack runghc --"
alias runhaskell="stack runghc --"

# OPAM configuration
source $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# Normalize path(trim tail slash)
thunnus.path.normalize $PATH
