# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="cloud"
ZSH_THEME="gallois"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(atom brew bundler cabal docker emacs git git-flow github heroku knife osx pyenv rails rbenv sbt vagrant virtualenv)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Generic
alias where="command -v"
alias j="jobs -l"
alias ll="ls -lh"
alias la="ls -lha"

## Development Setting
alias gh="cd ~/Repositories/github"

# Rails
alias dev="export RAILS_ENV=development; export RACK_ENV=development"
alias stg="export RAILS_ENV=staging; export RACK_ENV=staging"
alias prd="export RAILS_ENV=production; export RACK_ENV=production"
alias be="bundle exec"
alias bi="bundle install"
