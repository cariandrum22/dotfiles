let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <unstable> { config.allowUnfree = true; };
  base = [ ./haskell.nix ];
in {
  home = {
    packages = with pkgs; [
      # nix-shell
      any-nix-shell

      # Utility
      ghq
      direnv
      fzf
      silver-searcher
      nkf
      jq
      nix-prefetch-git
      dep2nix
      vgo2nix
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

      # DevOps
      docker-compose
      etcd
      kubectl
      kubernetes-helm
      vagrant
      azure-cli
      azure-storage-azcopy
      certbot
      awscli2
      eksctl
      saml2aws

      # Development Environment
      cmake
      gitAndTools.delta
      heroku
      unstable.cachix
      sqlite

      # Compiler and Runtime
      rustup
      go
      dotnet-sdk
      ruby_3_0
      rubocop
      python3
      nodejs-14_x
      jdk

      # Database Clients
      mysql-client
      mongodb-tools
      postgresql_10
    ];
  };

  imports = if builtins.currentSystem == "x86_64-darwin" then
    base
  else
    base ++ [ ./linux-desktop.nix ];
}
