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

      # Deep Learning
      cudatoolkit
      cudnn

      # GUI Applications
      polybarFull
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
      firefox
      keybase
      ark
      partition-manager
      slack
      robo3t
      teams
      _1password-gui
      steam
      wireshark
      spotify

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
      awscli2
      eksctl
      saml2aws

      # Development Environment
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
      nodejs-12_x

      # Database Clients
      mysql-client
      mongodb-tools
      postgresql_10
    ];
  };

  imports = [
    ./nix-thunk.nix
    ./haskell.nix
  ];
}
