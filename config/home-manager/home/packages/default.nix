{ pkgs, lib, ... }:

{
  imports = [
    ./azure.nix
  ];

  home = {
    packages = lib.flatten (
      with pkgs;
      [
        # Nix related
        cachix
        any-nix-shell
        nix-prefetch-git
        prefetch-npm-deps
        nixfmt
        nixd

        # Utility
        atuin
        zoxide
        carapace
        ghq
        fzf
        (callPackage ./rivendell.nix { inherit pkgs; })
        silver-searcher
        nkf
        jq
        bat
        lsd
        dust
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
        gnupg
        yubikey-manager
        yubikey-personalization
        rlwrap
        pandoc
        texlive.combined.scheme-full
        lsof
        colordiff
        cosign
        shellcheck
        shfmt
        unstable.goreleaser
        rclone
        ripgrep
        scrcpy
        tlaplus
        tree

        # DevOps
        docker-compose
        etcd_3_5
        kubectl
        kubernetes-helm
        krew
        kubelogin
        powershell
        certbot
        awscli2
        eksctl
        google-cloud-sdk
        k6
        redis

        # AI Tools
        (callPackage ./claude-code.nix { })
        (callPackage ./codex.nix { })
        (callPackage ./droid.nix { })
        (callPackage ./gemini-cli.nix { })
        claudius

        # Development toolchain
        cmake
        pkg-config
        clang-tools
        delta
        uv
        rustup
        unstable.gradle
        unstable.heroku

        # Compiler and Runtime
        unstable.go
        dotnet-sdk
        elan
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
        mariadb.client
        mongodb-tools
        postgresql

        # GUI Application
        slack
        zoom-us
        wireshark
        obsidian

        # Integrated Development Environment
        (with jetbrains; [
          datagrip
          idea
          ruby-mine
        ])
      ]
      ++ lib.optionals stdenv.isLinux [
        # tlaps requires vampire which fails to build on macOS (clang compatibility)
        tlaps
      ]
    );
  };
}
