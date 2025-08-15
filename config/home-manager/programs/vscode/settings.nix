{ pkgs }:

{
  userSettings = {
    "accessibility.signals.terminalBell" = {
      "sound" = "on";
    };
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
    "window.zoomLevel" = -1.0;
    "terminal" = {
      "integrated" = {
        "defaultProfile" = {
          linux = "tmux";
          osx = "tmux";
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
      "formatterPath" = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt";
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
    # TOML settings
    "[toml]" = {
      # Enhanced TOML for syntax highlighting and validation only
      # Formatting will be done by taplo via pre-commit hooks
      "editor.formatOnSave" = false;
      # Explicitly disable formatter to avoid conflicts with taplo
      "editor.defaultFormatter" = null;
    };
  };
}
