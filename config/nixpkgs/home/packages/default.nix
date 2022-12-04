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

      # DevOps
      docker-compose
      etcd
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
      cachix
      sqlite

      # Compiler and Runtime
      rustup
      unstable.go
      dotnet-sdk
      ruby_3_0
      rubocop
      python3
      nodejs-18_x
      jdk

      # Database Clients
      mysql-client
      mongodb-tools
      postgresql_14

      # GUI Application
      slack
      zoom-us
      vscode
      wireshark
      yubikey-personalization-gui

      # Development Environment
      jetbrains.datagrip
    ];
  };

  imports = synthetic;
}
