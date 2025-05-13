{ pkgs, lib, ... }:

let
  base = [
    ./haskell.nix
    ./azure.nix
  ];
  isDarwin = (import <nixpkgs> { }).stdenv.isDarwin;
  synthetic =
    if isDarwin then
      base ++ [ ./darwin.nix ]
    else
      base
      ++ [
        ./linux.nix
        ./linux-desktop.nix
      ];
in
{
  home = {
    packages = lib.flatten (
      with pkgs;
      [
        # Nix related
        cachix
        any-nix-shell
        nix-prefetch-git
        nixfmt-rfc-style
        nixd

        # Utility
        ghq
        fzf
        silver-searcher
        nkf
        jq
        bat
        lsd
        du-dust
        duf
        tldr
        glances
        gtop
        hyperfine
        gping
        procs
        httpie
        curlie
        mutagen
        yubikey-manager
        yubikey-personalization
        rlwrap
        pandoc
        lsof
        colordiff
        cosign
        shellcheck
        unstable.goreleaser
        rclone

        # DevOps
        docker-compose
        etcd_3_5
        kubectl
        kubernetes-helm
        krew
        kubelogin
        powershell
        certbot
        unstable.awscli2
        eksctl
        saml2aws
        google-cloud-sdk
        k6
        redis

        # Development Environment
        cmake
        pkg-config
        gitAndTools.delta
        heroku
        sqlite
        unstable.gradle
        clang-tools

        # Compiler and Runtime
        rustup
        unstable.go
        dotnet-sdk_8
        ruby_3_3
        rubocop
        python3
        nodejs-18_x
        jdk
        sbcl
        maxima

        # Language Server
        terraform-ls
        rubyPackages_3_3.ruby-lsp

        # Database Clients
        mysql-client
        mongodb-tools
        postgresql_14

        # GUI Application
        slack
        zoom-us
        wireshark
        obsidian

        # Integrated Development Environment
        (with jetbrains; [
          datagrip
          idea-ultimate
          ruby-mine
        ])
      ]
    );
  };

  imports = synthetic;
}
