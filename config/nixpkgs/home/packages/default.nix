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

      # DevOps
      unstable.nixops
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
      unstable.cachix
      sqlite

      # Compiler and Runtime
      rustup
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

  imports = if builtins.currentSystem == "x86_64-darwin" then
    base
  else
    base ++ [ ./linux-desktop.nix ];
}
