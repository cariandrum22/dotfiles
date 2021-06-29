let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <unstable> { config.allowUnfree = true; };
  base = [ ./nix-thunk.nix ./haskell.nix ];
in {
  home = {
    packages = with pkgs; [
      # nix-shell
      any-nix-shell

      # Utility
      ghq
      direnv
      fzf
      ag
      nkf
      jq
      nix-prefetch-git
      unstable.dep2nix
      unstable.vgo2nix
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

      # DevOps
      unstable.nixops
      docker-compose
      etcdctl
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
      gitAndTools.delta
      heroku
      unstable.cachix
      sqlite

      # Compiler and Runtime
      rustup
      go
      dotnet-sdk
      ruby_2_6
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
