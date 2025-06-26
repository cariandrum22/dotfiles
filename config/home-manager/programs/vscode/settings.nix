{ pkgs }:

{
  userSettings = {
    "window.zoomLevel" = -1.0;
    "editor" = {
      "emptySelectionClipboard" = true;
      "fontFamily" = "'Fira Code', 'FiraCode Nerd Font', Menlo, Monaco, 'Courier New', monospace";
      "fontSize" = 12;
      "fontLigatures" = true;
      "formatOnSave" = true;
      "formatOnSaveMode" = "file";
      "renderControlCharacters" = true;
      "suggestSelection" = "recentlyUsed";
      "acceptSuggestionOnEnter" = "off";
      "copyWithSyntaxHighlighting" = false;
      "largeFileOptimizations" = false;
      "detectIndentation" = true;
      "tabSize" = 2;
      "renderWhitespace" = "all";
      "inlineSuggest" = {
        "enabled" = true;
      };
      "wordWrap" = "on";
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
      "integrated" = {
        "defaultProfile" = {
          linux = "tmux";
          osx = "kitty";
        };
        "profiles" = {
          "tmux" = {
            "path" = "tmux";
            "args" = [
              "-c"
              "tmux"
              "new-session"
              "-A"
              "-s (string replace $HOME '~' $PWD)"
            ];
          };
        };
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
          "eto.d" = "linux";
          heilwig = "macOS";
        };
      };
    };
    "diffEditor" = {
      "ignoreTrimWhitespace" = false;
    };
    "vsintellicode" = {
      "modify" = {
        "editor" = {
          "suggestSelection" = "automaticallyOverrodeDefaultValue";
        };
      };
    };
    "haskell" = {
      "manageHLS" = "PATH";
      "formattingProvider" = "ormolu";
    };
    "rust-analyzer" = {
      "server" = {
        "path" = "${pkgs.rust-analyzer}/bin/rust-analyzer";
      };
    };
    "terraform" = {
      "languageServer.path" = "${pkgs.terraform-ls}/bin/terraform-ls";
      "experimentalFeatures.validateOnSave" = true;
    };
    "[terraform]" = {
      "editor.defaultFormatter" = "hashicorp.terraform";
    };
    "[terraform-vars]" = {
      "editor.defaultFormatter" = "hashicorp.terraform";
    };
    "[dockerfile]" = {
      "editor.defaultFormatter" = "ms-azuretools.vscode-docker";
    };
    "[typescript]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[typescriptreact]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "rubyLsp.rubyExecutablePath" = "${pkgs.ruby_3_3}/bin/ruby";
    "[python]" = {
      "editor.defaultFormatter" = "charliermarsh.ruff";
    };
    "python" = {
      "envFile" = "\${workspaceFolder}/.venv";
    };
    "nix" = {
      "enableLanguageServer" = true;
      "serverPath" = "${pkgs.nixd}/bin/nixd";
      "formatterPath" = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
    };
    "shellcheck" = {
      "executablePath" = "${pkgs.shellcheck}/bin/shellcheck";
      "customArgs" = [
        "--source-path=SCRIPTDIR"
        "--external-sources"
      ];
    };
    "shellformat" = {
      "path" = "${pkgs.shfmt}/bin/shellformat";
    };
    "extensions.verifySignature" = false;
  };
}
