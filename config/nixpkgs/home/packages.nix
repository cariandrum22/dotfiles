{ pkgs, ... }:

let
  pkgs = import <nixpkgs> {
    config.allowUnfree = true;
  };
  unstable = import <unstable> {
    config.allowUnfree = true;
  };
in {
  home = {
    packages = with pkgs; [
      # nix-shell
      any-nix-shell

      # Haskell
      ghc
      cabal-install
      stack

      # Haskell Project Management for Nix
      cabal2nix
      nix-prefetch-git

      # Deep Learning
      cudatoolkit
      cudnn

      # GUI Applications
      polybar
      dmenu
      rofi
      feh
      pavucontrol
      elementary-planner
      xmind
      ledger-live-desktop
      yubikey-manager-qt
      remmina
      libreoffice
      google-chrome
      keybase
      ark
      partition-manager
      slack
      robo3t
      teams
      _1password-gui
      steam
      wireshark

      # Utility
      ghq
      direnv
      fzf
      ag
      google-drive-ocamlfuse
      yubikey-manager
      nkf
      jq

      # DevOps
      nixops
      docker-compose
      etcdctl
      kubectl
      kubernetes-helm
      vagrant
      vault
      terraform
      azure-cli
      azure-storage-azcopy
      certbot
      awscli
      eksctl
      saml2aws

      # DevTools
      gitAndTools.delta
      heroku
      unstable.vscode
      jetbrains.datagrip
      unstable.cachix
      sqlite

      # Compiler and Runtime
      rustup
      jdk
      go
      dotnet-sdk
      ruby_2_6
      rubocop
      python3
      nodejs_latest

      # Database Clients
      mysql-client
      mongodb-tools
      postgresql_10
    ];
  };
}
