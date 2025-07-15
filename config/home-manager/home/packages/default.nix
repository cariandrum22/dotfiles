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
        prefetch-npm-deps
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
        shfmt
        unstable.goreleaser
        rclone
        ripgrep
        _1password-cli

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
        google-cloud-sdk
        k6
        redis

        # AI Tools
        (unstable.claude-code.overrideAttrs (
          finalAttrs: oldAttrs: rec {
            version = "1.0.35";
            src = fetchzip {
              url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
              hash = "sha256-Lt79XxHrgy6rPAHBf1QtwjsKnrZmsKFeVFOvHwN4aOY=";
            };
          }
        ))
        (callPackage ./codex.nix { inherit pkgs; })
        (callPackage ./gemini-cli.nix { })

        # Development toolchain
        cmake
        pkg-config
        clang-tools
        gitAndTools.delta
        uv
        rustup
        unstable.gradle
        unstable.heroku

        # Compiler and Runtime
        unstable.go
        dotnet-sdk_8
        ruby_3_3
        rubocop
        python3
        nodejs_22
        jdk
        sbcl
        maxima

        # Language Server
        terraform-ls
        rubyPackages_3_3.ruby-lsp

        # Database Clients
        sqlite
        mysql-client
        mongodb-tools
        postgresql_15

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
