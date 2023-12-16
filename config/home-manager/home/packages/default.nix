let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <unstable> { config.allowUnfree = true; };

  base = [ ./haskell.nix ];
  synthetic = if pkgs.stdenv.isDarwin then base ++ [ ./darwin.nix ] else base ++ [ ./linux-desktop.nix ];
in
{
  home = {
    packages = with pkgs; [
      # Nix related
      any-nix-shell
      cachix
      rnix-lsp
      nix-prefetch-git
      nixpkgs-fmt
      nixfmt
      dep2nix

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

      # DevOps
      docker-compose
      etcd_3_5
      kubectl
      kubernetes-helm
      krew
      kubelogin
      vagrant
      azure-cli
      azure-storage-azcopy
      powershell
      certbot
      awscli2
      eksctl
      saml2aws
      google-cloud-sdk

      # Development Environment
      cmake
      pkg-config
      gitAndTools.delta
      heroku
      sqlite
      gradle
      clang-tools

      # Compiler and Runtime
      rustup
      unstable.go
      dotnet-sdk
      ruby_3_3
      rubocop
      python3
      nodejs-18_x
      jdk
      sbcl
      maxima

      # Language Server
      terraform-ls

      # Database Clients
      mysql-client
      mongodb-tools
      postgresql_14

      # GUI Application
      slack
      zoom-us
      wireshark

      # Development Environment
      jetbrains.datagrip
      jetbrains.idea-ultimate
    ];
  };

  imports = synthetic;
}
