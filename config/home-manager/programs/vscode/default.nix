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

  commit = "501baeb5e07b5a404f862d922bacb93771d91331";

  sha256 = {
    x86_64-linux = "1prshql24lbwgydbsv115vqqicgziixd3nllzc891a1f4f74vy53";
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
      version = "1.82.0-${commit}";
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
        version = "2023.8.1905";
        sha256 = "sha256-5NPAqCS/5bQwrQioulFskiNlIHmOgh1aWbDh1d2TFrc=";
      }
      {
        name = "nix-ide";
        publisher = "jnoortheen";
        version = "0.2.2";
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
        version = "0.4.1630";
        sha256 = "sha256-s3HeFdz6KkE7rd1vKXBrv9aXq6dUu5azrmGJqEfAUdc=";
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
        version = "1.0.36";
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
        version = "10.1.0";
        sha256 = "sha256-Iqz1O6odSzAfojCgGDwDA1YtnWU5Ei7vx9Qt25/1SLw=";
      }
      {
        name = "Go";
        publisher = "golang";
        version = "0.39.1";
        sha256 = "sha256-wOWouVz4mE4BzmgQOLQyVWsMadMqeUkFWHnruxStU0Q=";
      }
      {
        name = "ruby-lsp";
        publisher = "Shopify";
        version = "0.4.5";
        sha256 = "sha256-6VlNuyRTZ2bC/9pLT2C0Pqlh+1bQDB80C82nd9d4tiA=";
      }
      {
        name = "vscode-markdownlint";
        publisher = "DavidAnson";
        version = "0.51.0";
        sha256 = "sha256-F+lryIhSudDz68t1eGrfqI8EuoUUOWU5LfWj0IRCQyY=";
      }
      {
        name = "vscode-edit-csv";
        publisher = "janisdd";
        version = "0.8.2";
        sha256 = "sha256-DbAGQnizAzvpITtPwG4BHflUwBUrmOWCO7hRDOr/YWQ=";
      }
      {
        name = "even-better-toml";
        publisher = "tamasfe";
        version = "0.19.2";
        sha256 = "sha256-MqSQarNThbEf1wHDTf1yA46JMhWJN46b08c7tV6+1nU=";
      }
      {
        name = "Bookmarks";
        publisher = "alefragnani";
        version = "13.4.1";
        sha256 = "sha256-MFr5oePj1gqPXkUi+kvXHhgckw28EAbM48Q3p3oWKCs=";
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
        version = "2.20.5";
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
        version = "3.13.0";
        sha256 = "sha256-HslBfJsKDUGq+xdCBfYlxsWJ4xOB9flfyO2aUCqwmYM=";
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
        version = "0.5.7";
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
        version = "0.8.17";
        sha256 = "sha256-u3VcpgLKiEeUr1I6w71wleKyaO6v0gmHiw5Ama6fv88=";
      }
      {
        name = "vscode-graphql-syntax";
        publisher = "GraphQL";
        version = "1.2.2";
        sha256 = "sha256-qazU0UyZ9de6Huj2AYZqqBo4jVW/ZQmFJhV7XXAblxo=";
      }
      {
        name = "terraform";
        publisher = "HashiCorp";
        version = "2.27.2023071109";
        sha256 = "sha256-RXQZu0J4UJwFl1e00cWNX5PeorfOXQ1AO+xmtQUmoxg=";
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
        version = "2.10.0";
        sha256 = "sha256-n35OwKaSF0CBhw7n+/05h/O1jalZ5pp5wNqPmWF1bH4=";
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
        version = "1.10.0";
        sha256 = "sha256-Mfsf7nxcKMY0Qk0v1jh/mM22fhyKl76OVRrtCCztQQo=";
      }
      {
        name = "latex-workshop";
        publisher = "James-Yu";
        version = "9.13.4";
        sha256 = "sha256-5LGMbkvAE4QJiH4z/6eSRPMFSgd9mZ8BpEwIB+en9zk=";
      }
      {
        name = "ruff";
        publisher = "charliermarsh";
        version = "2023.34.0";
        sha256 = "sha256-KOntjiE+n1yf9047XDldGg2pT+zknI/aEg6h71LwEB8=";
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
        version = "0.33.1";
        sha256 = "sha256-ICQH/0OiNPgHOfj+bWk53ynqTZnplV4j4vTRoln2pVI=";
      }
      {
        name = "copilot";
        publisher = "GitHub";
        version = "1.105.350";
        sha256 = "sha256-OuWENMzWkMHLwFKbc/9J7/PVej21afCUFC3SBg9twOg=";
      }
      {
        name = "copilot-chat";
        publisher = "GitHub";
        version = "0.7.2023081801";
        sha256 = "sha256-4n28k0pqHEluL7mbsTfMMkePfRK5U5MzMNhLTRogoVY=";
      }
      {
        name = "cmake";
        publisher = "twxs";
        version = "0.0.17";
        sha256 = "sha256-CFiva1AO/oHpszbpd7lLtDzbv1Yi55yQOQPP/kCTH4Y=";
      }
    ] ++ haskell ++ purescript ++ java ++ microsoft);
  } // settings;
}
