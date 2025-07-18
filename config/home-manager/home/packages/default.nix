{ pkgs, lib, ... }:

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
            version = "1.0.51";
            src = fetchzip {
              url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
              hash = "sha256-sAILRsi8ZViMfcpqykfnFQzHTJHRwRSZz45otMqa4U0=";
            };
          }
        ))
        (callPackage ./codex.nix { inherit pkgs; })
        (unstable.gemini-cli.overrideAttrs (
          finalAttrs: oldAttrs: {
            version = "0.1.12";
            src = pkgs.fetchFromGitHub {
              owner = "google-gemini";
              repo = "gemini-cli";
              tag = "v${finalAttrs.version}";
              hash = "sha256-7StuYqKGnTTZY3BKK3X1kWNReRUfyvhfH3wGw0Pz2zM=";
              postFetch = ''
                ${lib.getExe pkgs.npm-lockfile-fix} $out/package-lock.json
              '';
            };

            npmDeps = pkgs.fetchNpmDeps {
              inherit (finalAttrs) src;
              hash = "sha256-yt1Z/atE07vt27OdiLHPV1ZSHJ80zkGkcuro7rJxOrc";
            };
          }
        ))

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
}
