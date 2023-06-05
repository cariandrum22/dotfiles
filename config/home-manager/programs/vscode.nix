{ lib, pkgs, ... }:

let
  unstable = import <unstable> { config.allowUnfree = true; };
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = unstable.vscode;
    extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
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
        version = "4.28.0";
        sha256 = "sha256-DO3dcJPk+TMhfb0IJ/eTB7nIKfyCXIiyhZFBpZjJzsM=";
      }
      {
        name = "gitlens";
        publisher = "eamodio";
        version = "2023.6.505";
        sha256 = "sha256-K79F0yxY6Bz8gnL0eBd3P48cx67bEv7ewrw46JrQ/tA=";
      }
      {
        name = "nix-ide";
        publisher = "jnoortheen";
        version = "0.2.1";
        sha256 = "sha256-yC4ybThMFA2ncGhp8BYD7IrwYiDU3226hewsRvJYKy4=";
      }
      {
        name = "haskell";
        publisher = "haskell";
        version = "2.2.4";
        sha256 = "sha256-yJwQBs0Xo1Vn5Y2FEtQgOjKVfkhcJNWafYhMocRyW+M=";
      }
      {
        name = "language-haskell";
        publisher = "justusadam";
        version = "3.6.0";
        sha256 = "sha256-rZXRzPmu7IYmyRWANtpJp3wp0r/RwB7eGHEJa7hBvoQ=";
      }
      {
        name = "Lisp";
        publisher = "mattn";
        version = "0.1.12";
        sha256 = "sha256-x6aFrcX0YElEFEr0qA669/LPlab15npmXd5Q585pIEw=";
      }
      {
        name = "ide-purescript";
        publisher = "nwolverson";
        version = "0.26.1";
        sha256 = "sha256-ccTuoDSZKf1WsTRX2TxXeHy4eRuOXsAc7rvNZ2b56MU=";
      }
      {
        name = "language-purescript";
        publisher = "nwolverson";
        version = "0.2.8";
        sha256 = "sha256-2uOwCHvnlQQM8s8n7dtvIaMgpW8ROeoUraM02rncH9o==";
      }
      {
        name = "dhall-lang";
        publisher = "dhall";
        version = "0.0.4";
        sha256 = "sha256-7vYQ3To2hIismo9IQWRWwKsu4lXZUh0Or89WDLMmQGk=";
      }
      {
        name = "vscode-dhall-lsp-server";
        publisher = "dhall";
        version = "0.0.4";
        sha256 = "sha256-WopWzMCtiiLrx3pHNiDMZYFdjS359vu3T+6uI5A+Nv4=";
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
        name = "vscode-docker";
        publisher = "ms-azuretools";
        version = "1.25.1";
        sha256 = "sha256-vEDmGrXgmEtZpsTdfYNL3MzuoJgh5zz+Z+ulMWI1EOQ=";
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
        name = "vscode-eslint";
        publisher = "dbaeumer";
        version = "2.4.1";
        sha256 = "sha256-RitdNF/fOJQxheJWCCS4v1pJvjeo/q+w1F6ERAHmtQc=";
      }
      {
        name = "cpptools";
        publisher = "ms-vscode";
        version = "1.16.0";
        sha256 = "sha256-bHrbdsGb9VW/R2rKGtTg6mov/BHJhRkHljAzIsvSpP4=";
      }
      {
        name = "cpptools-themes";
        publisher = "ms-vscode";
        version = "2.0.0";
        sha256 = "sha256-YWA5UsA+cgvI66uB9d9smwghmsqf3vZPFNpSCK+DJxc=";
      }
      {
        name = "cmake";
        publisher = "twxs";
        version = "0.0.17";
        sha256 = "sha256-CFiva1AO/oHpszbpd7lLtDzbv1Yi55yQOQPP/kCTH4Y=";
      }
      {
        name = "cmake-tools";
        publisher = "ms-vscode";
        version = "1.15.12";
        sha256 = "sha256-1gVx7QAQPN/lcQCfmR1e5Yf+UFtaHQ+eWhxyr4FIvIQ=";
      }
      {
        name = "makefile-tools";
        publisher = "ms-vscode";
        version = "0.8.1";
        sha256 = "sha256-VCrMhTcO1EjK93wPVcZBwB3PPynOeSABS6hokgtaXBc=";
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
        name = "python";
        publisher = "ms-python";
        version = "2023.9.11561008";
        sha256 = "sha256-L2lBrp/vM/lIuIRlV/RQ2pwCGkCfLOUu7aRxsKjRchw=";
      }
      {
        name = "terraform";
        publisher = "HashiCorp";
        version = "2.26.2023051115";
        sha256 = "sha256-VDaxfDdCQT1f5A1xt/gcc9pNE8H38l7/Vz8p4Yhq0DU=";
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
    ];
    userSettings = {
      "window.zoomLevel" = -1.0;
      "editor" = {
        "fontFamily" = "'Fira Code', 'FiraCode Nerd Font', Menlo, Monaco, 'Courier New', monospace";
        "fontSize" = 12;
        "fontLigatures" = true;
        "formatOnSave" = true;
        "renderControlCharacters" = true;
        "suggestSelection" = "recentlyUsed";
        "acceptSuggestionOnEnter" = "off";
        "copyWithSyntaxHighlighting" = false;
        "largeFileOptimizations" = false;
        "detectIndentation" = true;
        "tabSize" = 8;
        "renderWhitespace" = "all";
      };
      "workbench" = {
        "welcome.enabled" = false;
        "colorTheme" = "Nord";
        "iconTheme" = "material-icon-theme";
        "wordWrap" = "on";
        "fontLigatures" = true;
        "formatOnSave" = true;
        "editorAssociations" = {
          "git-rebase-todo" = "default";
        };
      };
      "terminal" = {
        "explorerKind" = "external";
        "external" = {
          "linuxExec" = "kitty";
          "osxExec" = "kitty.app";
        };
        "integrated" = {
          "fontSize" = 12;
        };
      };
      "files" = {
        "trimTrailingWhitespace" = true;
        "trimFinalNewlines" = true;
        "insertFinalNewline" = true;
        "maxMemoryForLargeFilesMB" = 16384;
        "associations" = {
          Vagrantfile = "ruby";
        };
      };
      "remote" = {
        "SSH" = {
          "useLocalServer" = false;
          "remotePlatform" = {
            eto = "linux";
            heilwig = "macOS";
          };
        };
      };
      "diffEditor.ignoreTrimWhitespace" = false;
      "vsintellicode.modify.editor.suggestSelection" = "automaticallyOverrodeDefaultValue";
      "haskell" = {
        "manageHLS" = "PATH";
        "formattingProvider" = "ormolu";
      };
      "rust-analyzer.server.path" = "rust-analyzer";
      "shellcheck.customArgs" = [
        "--source-path=SCRIPTDIR"
        "--external-sources"
      ];
      "[typescriptreact]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
    };
  };
}
