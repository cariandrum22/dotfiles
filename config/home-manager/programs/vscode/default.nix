{ lib, pkgs, ... }:

let
  haskell = import ./haskell.nix;
  purescript = import ./purescript.nix;
  java = import ./java.nix;
  microsoft = import ./microsoft.nix;
  settings = import ./settings.nix;

  # Start of the code segment borrowed from nixpkgs
  # (https://github.com/NixOS/nixpkgs/blob/nixos-23.05/pkgs/applications/editors/vscode/vscode.nix)
  # The original code is licensed under the MIT license.
  inherit (pkgs.stdenv.hostPlatform) system;

  plat = {
    x86_64-linux = "linux-x64";
    x86_64-darwin = "darwin";
    aarch64-darwin = "darwin-arm64";
  }.${system};

  archive_fmt = if pkgs.stdenv.isDarwin then "zip" else "tar.gz";

  sha256 = {
    x86_64-linux = "13rbbn632h3xvzd3jsiaxls3xkncfz1lah0fl5za3n4w4c2qynzr";
    x86_64-darwin = "0rlcilz27zhwv3xjgrl03yzgwa4ky97c7ipi16mjh7xvms2vdshj";
    aarch64-darwin = "0xfh4i80n0vn6s4blkxqdpg32nlwl8pdlmz45ymnswn0x1lj8ysq";
  }.${system};
  # End of the borrowed nixpkgs code segment from above
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.82.0";
      src = (builtins.fetchurl {
        name = "${pname}-${version}.${archive_fmt}";
        url = "https://code.visualstudio.com/sha/download?build=insider&os=${plat}";
        inherit sha256;
      });
      buildInputs = oldAttrs.buildInputs ++ [ pkgs.krb5 ];
      runtimeDependencies = lib.optionals pkgs.stdenv.isLinux (oldAttrs.runtimeDependencies ++ [ pkgs.libsecret ]);
      urlHandlerDesktopItem = pkgs.makeDesktopItem {
        name = "code-insiders-url-handler";
        desktopName = "Visual Studio Code - Insiders - URL Handler";
        comment = "Code Editing. Redefined.";
        genericName = "Text Editor";
        exec = "code-insiders" + " --open-url %U";
        icon = "code";
        startupNotify = true;
        categories = [ "Utility" "TextEditor" "Development" "IDE" ];
        mimeTypes = [ "x-scheme-handler/${pname}" ];
        keywords = [ "vscode" ];
        noDisplay = true;
      };
    });
    extensions = with pkgs.vscode-extensions; [
      ms-dotnettools.csharp
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace ([
      {
        name = "emacs";
        publisher = "VSCodeEmacs";
        version = "0.1.3";
        sha256 = "sha256-oVr1svDkboqIiibXcxmzI4M0iJJhuLWm0wGKKdlj8L0=";
      }
      {
        name = "nord-visual-studio-code";
        publisher = "arcticicestudio";
        version = "0.19.0";
        sha256 = "sha256-awbqFv6YuYI0tzM/QbHRTUl4B2vNUdy52F4nPmv+dRU=";
      }
      {
        name = "material-icon-theme";
        publisher = "PKief";
        version = "4.29.0";
        sha256 = "sha256-YqleqYSpZuhGFGkNo3FRLjiglxX+iUCJl69CRCY/oWM";
      }
      {
        name = "gitlens";
        publisher = "eamodio";
        version = "2023.8.505";
        sha256 = "sha256-pt31XvvwitFk7MiZ2ZOZ2boa1YikJWZa2JRs/QxOl50=";
      }
      {
        name = "nix-ide";
        publisher = "jnoortheen";
        version = "0.2.1";
        sha256 = "sha256-yC4ybThMFA2ncGhp8BYD7IrwYiDU3226hewsRvJYKy4=";
      }
      {
        name = "Lisp";
        publisher = "mattn";
        version = "0.1.12";
        sha256 = "sha256-x6aFrcX0YElEFEr0qA669/LPlab15npmXd5Q585pIEw=";
      }
      {
        name = "rust-analyzer";
        publisher = "rust-lang";
        version = "0.4.1540";
        sha256 = "sha256-WHciY9pXbOB/qWlyaW+lXFstF6R7JhjsY72JmS23jhA=";
      }
      {
        name = "Idris";
        publisher = "zjhmale";
        version = "0.9.8";
        sha256 = "sha256-t2nnLWcK1oPxSBhKKw7t39sYVDKwPx5zK87C5V8O0LU=";
      }
      {
        name = "vscode-fish";
        publisher = "bmalehorn";
        version = "1.0.33";
        sha256 = "sha256-ZQlG+HrjU4DFfpyiY8o0/ayDms6MGEObW8pV1Lmr5/Y=";
      }
      {
        name = "vscode-tailwindcss";
        publisher = "bradlc";
        version = "0.9.11";
        sha256 = "sha256-S+W3DvYdFbXbK7lCrQz6ZxeujtHRrT74Ex/IzGUG44k=";
      }
      {
        name = "vscode-colorize";
        publisher = "kamikillerto";
        version = "0.11.1";
        sha256 = "sha256-VS+L994j4xZNUWicIU/L00anjAHl2Xztx2Ia9GVYAsE=";
      }
      {
        name = "path-intellisense";
        publisher = "christian-kohler";
        version = "2.8.4";
        sha256 = "sha256-FEBYcjJHOwmxVHhhyxqOpk/V6hvtMkhkvLVpmJCMSZw=";
      }
      {
        name = "prettier-vscode";
        publisher = "esbenp";
        version = "9.13.0";
        sha256 = "sha256-Iqz1O6odSzAfojCgGDwDA1YtnWU5Ei7vx9Qt25/1SLw=";
      }
      {
        name = "Go";
        publisher = "golang";
        version = "0.38.0";
        sha256 = "sha256-wOWouVz4mE4BzmgQOLQyVWsMadMqeUkFWHnruxStU0Q=";
      }
      {
        name = "ruby-lsp";
        publisher = "Shopify";
        version = "0.3.1";
        sha256 = "sha256-K6h9x1gB6BWcy3MVhjNSnKBOi2HAN7ePABq6lW692Iw=";
      }
      {
        name = "vscode-markdownlint";
        publisher = "DavidAnson";
        version = "0.50.0";
        sha256 = "sha256-F+lryIhSudDz68t1eGrfqI8EuoUUOWU5LfWj0IRCQyY=";
      }
      {
        name = "vscode-edit-csv";
        publisher = "janisdd";
        version = "0.7.6";
        sha256 = "sha256-42ydpKamn/T26w8N2WzhCIw/UAlmNcKbfVQaj5WOWK0=";
      }
      {
        name = "even-better-toml";
        publisher = "tamasfe";
        version = "0.19.0";
        sha256 = "sha256-MqSQarNThbEf1wHDTf1yA46JMhWJN46b08c7tV6+1nU=";
      }
      {
        name = "Bookmarks";
        publisher = "alefragnani";
        version = "13.3.1";
        sha256 = "sha256-CZSFprI8HMQvc8P9ZH+m0j9J6kqmSJM1/Ik24ghif2A=";
      }
      {
        name = "change-case";
        publisher = "wmaurer";
        version = "1.0.0";
        sha256 = "sha256-tN/jlG2PzuiCeERpgQvdqDoa3UgrUaM7fKHv6KFqujc=";
      }
      {
        name = "code-spell-checker";
        publisher = "streetsidesoftware";
        version = "2.20.4";
        sha256 = "sha256-GOXKXZPEynyqRUUY0pdNwt+141kJleg74IbCP4/34R8=";
      }
      {
        name = "cucumber-official";
        publisher = "CucumberOpen";
        version = "1.7.0";
        sha256 = "sha256-a6GjMUsWKWiBPY3cANOwzFljoswMEoAQjxPvPquw4c0=";
      }
      {
        name = "code-d";
        publisher = "webfreak";
        version = "0.23.2";
        sha256 = "sha256-v/Dck4gE9kRkfIWPAkUmPqewyTVVKrBgAjpNuCROClE=";
      }
      {
        name = "errorlens";
        publisher = "usernamehw";
        version = "3.11.1";
        sha256 = "sha256-yCX+imIaSww3LJ/v3ofs3qBVAXVkTT2Njha8oaqrAX8=";
      }
      {
        name = "Kotlin";
        publisher = "mathiasfrohlich";
        version = "1.7.1";
        sha256 = "sha256-MuAlX6cdYMLYRX2sLnaxWzdNPcZ4G0Fdf04fmnzQKH4=";
      }
      {
        name = "scala";
        publisher = "scala-lang";
        version = "0.5.6";
        sha256 = "sha256-eizIPazqEb27aQ+o9nTD1O58zbjkHYHNhGjK0uJgnwA=";
      }
      {
        name = "Sbt";
        publisher = "itryapitsin";
        version = "0.1.7";
        sha256 = "sha256-PAXCvfV7snrVdLUSLPXA9iwQFEFgQtkcS8tkjOho/YA=";
      }
      {
        name = "vscode-graphql";
        publisher = "GraphQL";
        version = "0.8.7";
        sha256 = "sha256-u3VcpgLKiEeUr1I6w71wleKyaO6v0gmHiw5Ama6fv88=";
      }
      {
        name = "vscode-graphql-syntax";
        publisher = "GraphQL";
        version = "1.1.0";
        sha256 = "sha256-qazU0UyZ9de6Huj2AYZqqBo4jVW/ZQmFJhV7XXAblxo=";
      }
      {
        name = "terraform";
        publisher = "HashiCorp";
        version = "2.26.2023051115";
        sha256 = "sha256-8RvVJh0GgTRMUolmH1j1qA5qZoBopkS2tv9s227cSic=";
      }
      {
        name = "HCL";
        publisher = "HashiCorp";
        version = "0.3.2";
        sha256 = "sha256-cxF3knYY29PvT3rkRS8SGxMn9vzt56wwBXpk2PqO0mo=";
      }
      {
        name = "i18n-ally";
        publisher = "Lokalise";
        version = "2.9.1";
        sha256 = "sha256-PMQxdeiLXVrvYUvLfgcuracVNIRIbshdl7CU2D3a/jU=";
      }
      {
        name = "icon-fonts";
        publisher = "idleberg";
        version = "2.5.4";
        sha256 = "sha256-JKg5NOQ+y2uDA/hHDHfbFiF6eefTOdCxUcjLJyetaXk=";
      }
      {
        name = "mesonbuild";
        publisher = "mesonbuild";
        version = "1.9.0";
        sha256 = "sha256-HflSqDGZEHf7p2MnqXmBinwfbYh4A+6KJcvD7mDyp0k=";
      }
      {
        name = "latex-workshop";
        publisher = "James-Yu";
        version = "9.11.4";
        sha256 = "sha256-qRPk0aQJCr4L3GIE8DhvBwWAJqK7mlBSgYNecBk42gM=";
      }
      {
        name = "ruff";
        publisher = "charliermarsh";
        version = "2023.17.11351528";
        sha256 = "sha256-GMa3xMrPz3EEyPWTNSLzdZU3zKcVvbLmMVdStSHc8V4=";
      }
      {
        name = "shell-format";
        publisher = "foxundermoon";
        version = "7.2.5";
        sha256 = "sha256-kfpRByJDcGY3W9+ELBzDOUMl06D/vyPlN//wPgQhByk=";
      }
      {
        name = "shellcheck";
        publisher = "timonwong";
        version = "0.33.0";
        sha256 = "sha256-dDd2Pf26XWehPaIxHBsZ2JDTylvcEwQVffqV+X03wlc=";
      }
      {
        name = "copilot";
        publisher = "GitHub";
        version = "1.98.280";
        sha256 = "sha256-OgjZgWNHVLIDGqDivH+zOL3q6f0YY7L431gbJiwmcd4=";
      }
      {
        name = "copilot-chat";
        publisher = "GitHub";
        version = "0.5.2023072001";
        sha256 = "sha256-l0YIe1Q7IFptQnw1dCZRqYSlWQzmS8X3bqKJ2b9D71Q=";
      }
    ] ++ haskell ++ purescript ++ java ++ microsoft);
  } // settings;
}
