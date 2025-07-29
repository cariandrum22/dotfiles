{
  description = "Personal dotfiles configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";

    claudius = {
      url = "github:cariandrum22/claudius";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad = {
      url = "path:./xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    home-manager-config = {
      url = "path:./config/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.claudius.follows = "claudius";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      flake-utils,
      pre-commit-hooks,
      claudius,
      xmonad,
      home-manager-config,
    }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-darwin"
      ]
      (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
          inherit (pkgs) lib;

          # Haskell and XMonad development packages
          haskellPackages =
            with pkgs;
            [
              zlib
              ghcid
              cabal-install
              ormolu
              (haskell-language-server.override { supportedGhcVersions = [ "984" ]; })
            ]
            ++ (with pkgs.xorg; [
              libX11
              libXext
              libXrandr
              libXScrnSaver
            ])
            ++ (with pkgs.haskellPackages; [ cabal-fmt ])
            ++ lib.optionals stdenv.isLinux [ alsa-lib ];

          # General development tools
          devTools = with pkgs; [
            # Nix development tools
            nil
            nix-tree
            nix-diff
            pkgs-unstable.nixfmt-rfc-style
            deadnix
            statix

            # Editor support
            editorconfig-core-c

            # Git tools
            git
            gh

            # General development tools
            ripgrep
            fd
            jq

            # Python tools
            ruff

            # CI/CD tools
            act
          ];
        in
        {
          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                # Nix formatting and linting
                nixfmt-rfc-style = {
                  enable = true;
                  package = pkgs-unstable.nixfmt-rfc-style;
                };
                deadnix = {
                  enable = true;
                  settings = {
                    noLambdaArg = true;
                    noLambdaPatternNames = true;
                    noUnderscore = true; # Enforce explicit naming
                    quiet = false; # Show all potential issues
                  };
                };
                statix = {
                  enable = true;
                  settings = {
                    format = "stderr"; # Default error format
                  };
                };

                # Markdown
                markdownlint = {
                  enable = true;
                  settings = {
                    configuration = {
                      # Start with default rules
                      default = true;
                      # Line length with reasonable limits
                      MD013 = {
                        line_length = 100;
                        heading_line_length = 100;
                        code_block_line_length = 120; # Code can be longer
                        tables = false; # Tables are exempt
                      };
                      # Necessary exceptions for technical documentation
                      MD033 = false; # Allow inline HTML (badges, etc.)
                      MD041 = false; # First line doesn't need to be heading
                      MD014 = false; # Allow $ without showing output
                      # Stricter rules for consistency
                      MD003 = {
                        style = "atx";
                      }; # ATX-style headings only
                      MD004 = {
                        style = "dash";
                      }; # Consistent unordered list style
                      MD007 = {
                        indent = 2;
                      }; # 2-space list indentation
                      MD009 = {
                        br_spaces = 2;
                        strict = true;
                      }; # Trailing spaces
                      MD010 = true; # No hard tabs
                      MD012 = {
                        maximum = 1;
                      }; # Max 1 consecutive blank line
                      MD022 = true; # Headings surrounded by blank lines
                      MD024 = {
                        siblings_only = false;
                      }; # No duplicate headings
                      MD025 = {
                        level = 1;
                        front_matter_title = "^\\s*title\\s*[:=]";
                      }; # Single H1
                      MD026 = true; # No trailing punctuation in headings
                      MD030 = true; # Spaces after list markers
                      MD031 = true; # Fenced code blocks surrounded by blank lines
                      MD032 = true; # Lists surrounded by blank lines
                      MD034 = true; # No bare URLs
                      MD040 = true; # Fenced code blocks must have language
                      MD046 = {
                        style = "fenced";
                      }; # Code block style
                      MD048 = {
                        style = "backtick";
                      }; # Code fence style
                      MD049 = {
                        style = "asterisk";
                      }; # Emphasis style
                      MD050 = {
                        style = "asterisk";
                      }; # Strong style
                    };
                  };
                };
                prettier = {
                  enable = true;
                  types_or = [
                    "markdown"
                    "yaml"
                  ];
                  excludes = [ "^.pre-commit-config\\.yaml$" ];
                  settings = {
                    prose-wrap = "always"; # Wrap markdown at print width
                    print-width = 100;
                    tab-width = 2;
                    use-tabs = false;
                    trailing-comma = "all";
                  };
                };

                # EditorConfig compliance check
                editorconfig-checker = {
                  enable = true;
                  always_run = true;
                };

                # Shell script linting
                shellcheck = {
                  enable = true;
                  # SC1091 is about following sourced files, which exist at runtime
                  entry = "${pkgs.shellcheck}/bin/shellcheck -e SC1091";
                };

                # Fish shell syntax check
                fish = {
                  enable = true;
                  entry = "${pkgs.fish}/bin/fish --no-execute";
                  files = "\\.fish$";
                };

                # Python linting (for any Python scripts in dotfiles)
                ruff = {
                  enable = true;
                };

                # YAML linting (for GitHub Actions, etc.)
                yamllint = {
                  enable = true;
                  settings = {
                    preset = "default";
                    configuration = ''
                      extends: default
                      rules:
                        line-length:
                          max: 100
                          level: error
                        comments:
                          min-spaces-from-content: 2
                          require-starting-space: true
                        comments-indentation: enable
                        document-start: disable # GitHub Actions don't use ---
                        document-end: disable
                        indentation:
                          spaces: 2
                          indent-sequences: true
                          check-multi-line-strings: false
                        trailing-spaces: enable
                        truthy:
                          allowed-values: ['true', 'false', 'on'] # 'on' for GitHub Actions
                          check-keys: true
                          level: error
                        key-duplicates: enable
                        new-line-at-end-of-file: enable
                        new-lines:
                          type: unix
                        octal-values:
                          forbid-implicit-octal: true
                          forbid-explicit-octal: true
                        quoted-strings:
                          quote-type: any
                          required: only-when-needed
                          extra-required: []
                          extra-allowed: []
                          allow-quoted-quotes: false
                        empty-lines:
                          max: 1
                          max-start: 0
                          max-end: 0
                        brackets:
                          min-spaces-inside: 0
                          max-spaces-inside: 0
                        commas:
                          max-spaces-before: 0
                          min-spaces-after: 1
                          max-spaces-after: 1
                    '';
                  };
                };

                # GitHub Actions
                actionlint = {
                  enable = true;
                };

                # Commit message formatting
                commitizen = {
                  enable = true;
                  stages = [ "commit-msg" ];
                };
              };
            };
          };

          devShells = {
            default = pkgs.mkShell {
              inherit (self.checks.${system}.pre-commit-check) shellHook;
              buildInputs = haskellPackages ++ devTools ++ self.checks.${system}.pre-commit-check.enabledPackages;
            };
          };

          formatter = pkgs-unstable.nixfmt-rfc-style;
        }
      )
    // {
      # Re-export sub-flakes for convenience
      inherit (xmonad) packages apps overlays;
      inherit (home-manager-config) homeConfigurations;
    };
}
