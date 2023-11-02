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
      "renderControlCharacters" = true;
      "suggestSelection" = "recentlyUsed";
      "acceptSuggestionOnEnter" = "off";
      "copyWithSyntaxHighlighting" = false;
      "largeFileOptimizations" = false;
      "detectIndentation" = true;
      "tabSize" = 8;
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
    "terraform.languageServer.path" = "${pkgs.terraform-ls}/bin/terraform-ls";
    "terraform.experimentalFeatures.validateOnSave" = true;
    "[terraform]" = {
      "editor.defaultFormatter" = "hashicorp.terraform";
      "editor.formatOnSave" = true;
      "editor.formatOnSaveMode" = "file";
      "languageServer.path" = "terrarom-ls";
    };
    "[terraform-vars]" = {
      "editor.defaultFormatter" = "hashicorp.terraform";
      "editor.formatOnSave" = true;
      "editor.formatOnSaveMode" = "file";
    };
  };
}
