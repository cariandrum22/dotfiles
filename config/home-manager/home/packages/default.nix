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
        shfmt
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

        # AI Tools
        (unstable.claude-code.overrideAttrs (
          finalAttrs: oldAttrs: rec {
            version = "1.0.10";
            src = fetchzip {
              url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
              hash = "sha256-DcHlxeOOIKDe/Due952rH5qGT3KX+lUx84ctuj2/3aw=";
            };
            npmDepsHash = "sha256-2v9wCcaOgA3RezX/pnqigsn6XhKcqP2adM2IGRhiHgc=";
          }
        ))
        (unstable.codex.overrideAttrs (
          finalAttrs: oldAttrs: {
            version = "0.0.0-dev";
            src = fetchFromGitHub {
              owner = "openai";
              repo = "codex";
              rev = "44022db8d0c4a0cfe5b5b041ef0c1c8811ce6e12";
              hash = "sha256-S9xzyg6fC/uiW9xNv0HXW+GzYaJFKzjQn7ZTugc0tEM=";
            };

            pnpmDeps = pnpm_10.fetchDeps {
              inherit (finalAttrs)
                pname
                version
                src
                pnpmWorkspaces
                ;
              hash = "sha256-SyKP++eeOyoVBFscYi+Q7IxCphcEeYgpuAj70+aCdNA=";
            };
          }
        ))

        # Development Environment
        cmake
        pkg-config
        gitAndTools.delta
        unstable.heroku
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
        nodejs_22
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
