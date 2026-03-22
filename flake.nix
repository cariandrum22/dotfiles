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

    nixos-vscode-server = {
      url = "github:msteen/nixos-vscode-server";
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
      nixos-vscode-server,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      inherit (nixpkgs) lib;

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
            (import ./config/home-manager/overlays/hm-compat.nix)
            (import ./config/home-manager/overlays/darwin-workarounds.nix)
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
              X11
              xmonad
              xmonad-contrib
              xmonad-extras
            ];
            license = pkgs.lib.licenses.mit;
          }
        else
          null;

      resolveEnv =
        name: fallback:
        let
          value = builtins.getEnv name;
        in
        if value != "" then value else fallback;

      isDarwinSystem = system: lib.hasSuffix "-darwin" system;
      isLinuxSystem = system: lib.hasSuffix "-linux" system;

      resolveUsername = username: if username != null then username else resolveEnv "USER" "user";

      resolveHomeDirectory =
        {
          system,
          username,
          homeDirectory ? null,
        }:
        if homeDirectory != null then
          homeDirectory
        else
          resolveEnv "HOME" (if isDarwinSystem system then "/Users/${username}" else "/home/${username}");

      baseHomeModules =
        {
          username,
          homeDirectory,
        }:
        [
          ./config/home-manager/home.nix
          {
            home = {
              inherit username homeDirectory;
            };
          }
        ];

      darwinHomeModules = [
        ./config/home-manager/darwin-specific.nix
        ./config/home-manager/home/packages/darwin.nix
      ];

      linuxDesktopHomeModules = [
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

      linuxHeadlessHomeModules = [
        ./config/home-manager/home/packages/linux.nix
        ./config/home-manager/services/keybase.nix
        ./config/home-manager/services/vscode-server.nix
      ];

      platformHomeModules =
        {
          system,
          headless ? false,
        }:
        lib.optionals (isDarwinSystem system) darwinHomeModules
        ++ lib.optionals (isLinuxSystem system && headless) linuxHeadlessHomeModules
        ++ lib.optionals (isLinuxSystem system && !headless) linuxDesktopHomeModules;

      # Function to create a home configuration for a specific system
      mkHomeConfiguration =
        {
          system,
          username ? null,
          homeDirectory ? null,
          headless ? false,
        }:
        let
          pkgs = mkPkgs system;
          actualUsername = resolveUsername username;
          actualHomeDirectory = resolveHomeDirectory {
            inherit system homeDirectory;
            username = actualUsername;
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit system nixos-vscode-server;
          }
          // lib.optionalAttrs headless {
            isHeadless = true;
          };
          modules =
            baseHomeModules {
              username = actualUsername;
              homeDirectory = actualHomeDirectory;
            }
            ++ platformHomeModules {
              inherit system headless;
            };
        };

      mkFixedHomeConfiguration =
        {
          system,
          headless ? false,
        }:
        mkHomeConfiguration {
          inherit system headless;
          username = "user";
          homeDirectory = if isDarwinSystem system then "/Users/user" else "/home/user";
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
        "x86_64-linux-headless" = mkHomeConfiguration {
          system = "x86_64-linux";
          headless = true;
        };

        # Also provide fixed configurations for CI/reproducibility
        "user@x86_64-linux" = mkFixedHomeConfiguration {
          system = "x86_64-linux";
        };
        "user@x86_64-linux-headless" = mkFixedHomeConfiguration {
          system = "x86_64-linux";
          headless = true;
        };
        "user@aarch64-darwin" = mkFixedHomeConfiguration {
          system = "aarch64-darwin";
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
          pythonLintPackages = ps: [
            ps.ruff
            ps.mypy
          ];
          commonDevShellPackages = with pkgs; [
            git
            gh
            commitlint
            gnumake
            gnupg
            nixfmt
            nix-output-monitor
            deadnix
            statix
            shfmt
            shellcheck
            python3
            (python3.withPackages pythonLintPackages)
            act
          ];
        in
        {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
            packages = [
              pkgs.pre-commit
            ]
            ++ commonDevShellPackages
            ++ self.checks.${system}.pre-commit-check.enabledPackages;
          };

          # CI shell without shellHook to avoid pre-commit setup issues
          ci = pkgs.mkShell {
            packages = commonDevShellPackages ++ [
              pkgs.yamllint
              pkgs.actionlint
            ];
          };
        }
        // lib.optionalAttrs pkgs.stdenv.isLinux {
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
          markdownlintConfig = builtins.fromJSON (builtins.readFile ./.markdownlint.json);
          markdownAndDataTypes = [
            "markdown"
            "yaml"
            "json"
          ];
          shellTypes = [
            "bash"
            "sh"
          ];
          mkSystemHook =
            {
              name,
              entry,
              stages ? [ "pre-commit" ],
              pass_filenames ? false,
            }:
            {
              enable = true;
              inherit
                name
                entry
                stages
                pass_filenames
                ;
              language = "system";
            };
          mkBashHook =
            {
              name,
              script,
              stages ? [ "pre-commit" ],
              pass_filenames ? false,
            }:
            mkSystemHook {
              inherit name stages pass_filenames;
              entry = "${pkgs.bash}/bin/bash -c '${script}'";
            };
          hygieneHooks = {
            check-case-conflicts.enable = true;
            check-executables-have-shebangs.enable = true;
            check-merge-conflicts.enable = true;
            check-shebang-scripts-are-executable.enable = true;
            check-symlinks.enable = true;
            check-vcs-permalinks.enable = true;
          };
          lintHooks = {
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
                # Emacs directories (managed by Emacs packages)
                "emacs.d/elpa/"
                "emacs.d/auto-save-list/"
                "emacs.d/backup/"
                "emacs.d/snippets/"
                "emacs.d/vendor/"
                # Version control
                "\\.git/"
                # Binary and generated files
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
                "\\.gpg$"
                "\\.plist$"
              ];
            };
            fish = {
              enable = true;
              entry = "${pkgs.fish}/bin/fish --no-execute";
              files = "\\.fish$";
            };
            markdownlint = {
              enable = true;
              settings.configuration = markdownlintConfig;
            };
            nixfmt = {
              enable = true;
              package = pkgs.nixfmt;
            };
            prettier = {
              enable = true;
              types_or = markdownAndDataTypes;
              excludes = [ "^.pre-commit-config\\.yaml$" ];
            };
            ruff = {
              enable = true;
              excludes = [
                "emacs\\.d/snippets/.*\\.py$"
              ];
            };
            shellcheck = {
              enable = true;
              types_or = shellTypes;
              excludes = [ "\\.envrc" ];
            };
            shfmt = {
              enable = true;
              types_or = shellTypes;
              entry = "${pkgs.shfmt}/bin/shfmt -i 2 -w";
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
          commitHooks = {
            commitizen = {
              enable = true;
              stages = [ "commit-msg" ];
            };
            commitlint-pre-push = mkBashHook {
              name = "commitlint-pre-push";
              stages = [ "pre-push" ];
              script = "COMMITLINT_BIN=${pkgs.commitlint}/bin/commitlint ./scripts/commitlint-pre-push.sh";
            };
          };
        in
        {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            package = pkgs.pre-commit;
            hooks = hygieneHooks // lintHooks // commitHooks;
          };
        }
      );

      # Formatter for nix fmt command
      formatter = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
        in
        pkgs.nixfmt
      );
    };
}
