{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    myxmonad = {
      url = "path:../../xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      myxmonad,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # Function to create a home configuration for a specific system
      mkHomeConfiguration =
        {
          system,
          username ? builtins.getEnv "USER",
          homeDirectory ? builtins.getEnv "HOME",
        }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              myxmonad.overlays.default
              (import ./overlays/xmonad.nix)
              (final: prev: {
                unstable = import nixpkgs-unstable {
                  inherit system;
                  config.allowUnfree = true;
                };
              })
            ];
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules =
            [
              ./home.nix
              {
                home = {
                  inherit username homeDirectory;
                };
              }
            ]
            ++ nixpkgs.lib.optionals (system == "x86_64-darwin" || system == "aarch64-darwin") [
              ./darwin-specific.nix
              ./home/packages/darwin.nix
            ]
            ++ nixpkgs.lib.optionals (system == "x86_64-linux" || system == "aarch64-linux") [
              ./xsession.nix
              ./home/packages/linux.nix
              ./home/packages/linux-desktop.nix
              ./programs/rofi.nix
              ./services/picom.nix
              ./services/dunst.nix
              ./services/keybase.nix
              ./services/vscode-server.nix
              ./services/gpg-agent.nix
            ];
        };
    in
    {
      # Home configurations for each system
      homeConfigurations = {
        "user@x86_64-linux" = mkHomeConfiguration { system = "x86_64-linux"; };
        "user@aarch64-linux" = mkHomeConfiguration { system = "aarch64-linux"; };
        "user@x86_64-darwin" = mkHomeConfiguration { system = "x86_64-darwin"; };
        "user@aarch64-darwin" = mkHomeConfiguration { system = "aarch64-darwin"; };
      };

      # For convenience, provide a default package that builds the home configuration
      packages = forAllSystems (system: {
        default = self.homeConfigurations."user@${system}".activationPackage;
      });

      # Provide apps for easy activation
      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/activate";
        };
      });
    };
}
