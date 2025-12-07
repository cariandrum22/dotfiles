{
  description = "Personal dotfiles configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claudius = {
      url = "github:cariandrum22/claudius";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      pre-commit-hooks,
      claudius,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # Create pkgs with overlays for a specific system
      mkPkgs =
        system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (import ./config/home-manager/overlays/xmonad.nix)
            (_final: _prev: {
              unstable = import nixpkgs-unstable {
                inherit system;
                config.allowUnfree = true;
              };
              claudius = claudius.packages.${system}.default;
            })
          ];
        };

      # XMonad package builder (Linux only)
      mkXMonad =
        pkgs:
        if pkgs.stdenv.isLinux then
          let
            inherit (pkgs) haskellPackages;
          in
          haskellPackages.mkDerivation {
            pname = "myxmonad";
            version = "0.1.0.0";
            src = ./xmonad;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with haskellPackages; [
              base
              Cabal
              containers
              unordered-containers
              utf8-string
              X11
              xmonad
              xmonad-contrib
              xmonad-extras
            ];
            license = pkgs.lib.licenses.mit;
          }
        else
          null;

      # Function to create a home configuration for a specific system
      mkHomeConfiguration =
        {
          system,
          username ? null,
          homeDirectory ? null,
        }:
        let
          pkgs = mkPkgs system;
          # If not provided, try to get from environment (impure)
          actualUsername =
            if username != null then
              username
            else if builtins.getEnv "USER" != "" then
              builtins.getEnv "USER"
            else
              "user";
          actualHomeDirectory =
            if homeDirectory != null then
              homeDirectory
            else if builtins.getEnv "HOME" != "" then
              builtins.getEnv "HOME"
            else if system == "aarch64-darwin" || system == "x86_64-darwin" then
              "/Users/${actualUsername}"
            else
              "/home/${actualUsername}";
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit system;
          };
          modules = [
            ./config/home-manager/home.nix
            {
              home = {
                username = actualUsername;
                homeDirectory = actualHomeDirectory;
              };
            }
          ]
          ++ nixpkgs.lib.optionals (system == "x86_64-darwin" || system == "aarch64-darwin") [
            ./config/home-manager/darwin-specific.nix
            ./config/home-manager/home/packages/darwin.nix
          ]
          ++ nixpkgs.lib.optionals (system == "x86_64-linux" || system == "aarch64-linux") [
            ./config/home-manager/xsession.nix
            ./config/home-manager/home/packages/linux.nix
            ./config/home-manager/home/packages/linux-desktop.nix
            ./config/home-manager/programs/rofi.nix
            ./config/home-manager/services/picom.nix
            ./config/home-manager/services/dunst.nix
            ./config/home-manager/services/keybase.nix
            ./config/home-manager/services/vscode-server.nix
            ./config/home-manager/services/gpg-agent.nix
          ];
        };
      # Function to create a headless home configuration (Linux only)
      mkHeadlessHomeConfiguration =
        {
          system,
          username ? null,
          homeDirectory ? null,
        }:
        let
          pkgs = mkPkgs system;
          # If not provided, try to get from environment (impure)
          actualUsername =
            if username != null then
              username
            else if builtins.getEnv "USER" != "" then
              builtins.getEnv "USER"
            else
              "user";
          actualHomeDirectory =
            if homeDirectory != null then
              homeDirectory
            else if builtins.getEnv "HOME" != "" then
              builtins.getEnv "HOME"
            else
              "/home/${actualUsername}";
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit system;
            isHeadless = true;
          };
          modules = [
            ./config/home-manager/home.nix
            {
              home = {
                username = actualUsername;
                homeDirectory = actualHomeDirectory;
              };
            }
            # Headless configuration only includes essential services
            ./config/home-manager/home/packages/linux.nix
            ./config/home-manager/services/keybase.nix
            ./config/home-manager/services/vscode-server.nix
          ];
        };

      # Generate home configurations that auto-detect user
      mkAutoDetectConfigurations = builtins.listToAttrs (
        map (system: {
          name = system;
          value = mkHomeConfiguration { inherit system; };
        }) systems
      );
    in
    {
      # Home configurations for each system
      homeConfigurations = mkAutoDetectConfigurations // {
        # Headless Linux configuration (no GUI)
        "x86_64-linux-headless" = mkHeadlessHomeConfiguration {
          system = "x86_64-linux";
        };

        # Also provide fixed configurations for CI/reproducibility
        "user@x86_64-linux" = mkHomeConfiguration {
          system = "x86_64-linux";
          username = "user";
          homeDirectory = "/home/user";
        };
        "user@x86_64-linux-headless" = mkHeadlessHomeConfiguration {
          system = "x86_64-linux";
          username = "user";
          homeDirectory = "/home/user";
        };
        "user@aarch64-darwin" = mkHomeConfiguration {
          system = "aarch64-darwin";
          username = "user";
          homeDirectory = "/Users/user";
        };
      };

      # Packages for each system
      packages = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        {
          default = self.homeConfigurations.${system}.activationPackage;
          home-manager = self.homeConfigurations.${system}.activationPackage;
        }
        // nixpkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          myxmonad = mkXMonad pkgs;
          headless = self.homeConfigurations."${system}-headless".activationPackage;
        }
      );

      # Apps for easy activation
      apps = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/activate";
          };

          # Simplified switch that uses the current system's configuration
          switch = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "switch" ''
                #!/usr/bin/env bash
                set -e
                echo "Activating Home Manager configuration for ${system}..."
                exec ${self.packages.${system}.default}/activate
              ''
            );
          };
        }
      );

      # Development shells
      devShells = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.pre-commit-check) shellHook;

            packages =
              with pkgs;
              [
                git
                gh
                gnumake
                gnupg
                unstable.nixfmt-rfc-style
                nix-output-monitor
                deadnix
                statix
                shfmt
                shellcheck
                pre-commit
                python3
                (python3.withPackages (ps: [
                  ps.pre-commit-hooks
                  ps.ruff
                  ps.mypy
                ]))
                act
              ]
              ++ self.checks.${system}.pre-commit-check.enabledPackages;
          };

          # CI shell without shellHook to avoid pre-commit setup issues
          ci = pkgs.mkShell {
            packages = with pkgs; [
              git
              gh
              gnumake
              gnupg
              unstable.nixfmt-rfc-style
              nix-output-monitor
              deadnix
              statix
              shfmt
              shellcheck
              yamllint
              actionlint
              python3
              (python3.withPackages (ps: [
                ps.ruff
                ps.mypy
              ]))
              act
            ];
          };
        }
        // nixpkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          xmonad = pkgs.haskellPackages.shellFor {
            packages = _ps: [ (mkXMonad pkgs) ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-install
              ghc
              haskell-language-server
              ormolu
              hlint
            ];
          };
        }
      );

      # Pre-commit hooks configuration
      checks = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              commitizen = {
                enable = true;
                stages = [ "commit-msg" ];
              };
              actionlint.enable = true;
              deadnix = {
                enable = true;
                settings = {
                  noLambdaArg = true;
                  noLambdaPatternNames = true;
                  noUnderscore = true;
                  quiet = false;
                };
              };
              editorconfig-checker = {
                enable = true;
                excludes = [
                  "emacs.d/elpa/"
                  "emacs.d/auto-save-list/"
                  "emacs.d/backup/"
                  "emacs.d/snippets/"
                  "emacs.d/vendor/"
                  "\\.git/"
                  "\\.lock$"
                  "\\.db$"
                  "\\.ico$"
                  "\\.png$"
                  "\\.otf$"
                  "\\.ttf$"
                  "\\.pdf$"
                  "\\.gif$"
                  "\\.jpeg$"
                  "\\.jpg$"
                ];
              };
              markdownlint = {
                enable = true;
                settings.configuration = {
                  MD013 = {
                    line_length = 100; # Match prettier's print-width
                    code_blocks = false;
                    tables = false;
                  };
                  MD033 = {
                    allowed_elements = [
                      "div"
                      "img"
                    ];
                  };
                };
              };
              nixfmt-rfc-style = {
                enable = true;
                package = pkgs.unstable.nixfmt-rfc-style;
                # nixfmt-rfc-style's actual binary is "nixfmt", not "nixfmt-rfc-style"
                # This is needed because the package doesn't define meta.mainProgram
                entry = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt";
              };
              prettier = {
                enable = true;
                types_or = [
                  "markdown"
                  "yaml"
                  "json"
                ];
                excludes = [ "^.pre-commit-config\\.yaml$" ];
                settings = {
                  prose-wrap = "always";
                  print-width = 100;
                  tab-width = 2;
                  use-tabs = false;
                  trailing-comma = "all";
                };
              };
              ruff = {
                enable = true;
                excludes = [
                  "emacs\\.d/snippets/.*\\.py$"
                ];
              };
              shellcheck = {
                enable = true;
                types_or = [
                  "bash"
                  "sh"
                ];
                excludes = [ "\\.envrc" ];
              };
              shfmt = {
                enable = true;
                types_or = [
                  "bash"
                  "sh"
                ];
                entry = "${pkgs.shfmt}/bin/shfmt -i 2 -w";
              };
              # Fish shell syntax check
              fish = {
                enable = true;
                entry = "${pkgs.fish}/bin/fish --no-execute";
                files = "\\.fish$";
              };
              statix = {
                enable = true;
                settings = {
                  format = "stderr";
                };
              };
              taplo.enable = true;
              yamllint = {
                enable = true;
                excludes = [
                  ".github/labeler.yml"
                  "^\\.pre-commit-config\\.yaml$"
                ];
                settings = {
                  preset = "default";
                  configuration = ''
                    extends: default
                    rules:
                      line-length:
                        max: 120
                        level: warning
                      document-start:
                        present: false
                      truthy:
                        allowed-values: ["true", "false", "on", "off"]
                  '';
                };
              };
            };
          };
        }
      );

      # Formatter for nix fmt command
      formatter = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        pkgs.unstable.nixfmt-rfc-style
      );
    };
}
