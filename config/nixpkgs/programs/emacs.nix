{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/86ea3268b55bb632de43a80a37501a3d05cdb224.zip";
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = (epkgs:
      (with epkgs; [
        use-package
        swiper
        counsel
        projectile
        counsel-projectile
        ag
        powerline
        which-key
        font-lock-plus
        flycheck
        magit
        git-gutter
        ddskk
        wrap-region
        nord-theme
        company
        company-shell
        lsp-mode
        lsp-ui
        lsp-ivy
        lsp-treemacs
        haskell-mode
        lsp-haskell
        dockerfile-mode
        fish-mode
        jinja2-mode
        markdown-mode
        mmm-mode
        nix-mode
        toml-mode
        web-mode
        yaml-mode
      ]));
  };

  home.file = {
    ".emacs.el" = { source = ../../../emacs.el; };
    ".emacs.d" = {
      source = ../../../emacs.d;
      recursive = true;
    };
  };

  programs.git.ignores = [
    # -*- mode: gitignore; -*-
    "*~"
    "#*#"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "*.elc"
    "auto-save-list"
    "tramp"
    ".#*"

    # Org-mode
    ".org-id-locations"
    "*_archive"

    # flymake-mode
    "*_flymake.*"

    # eshell files
    "/eshell/history"
    "/eshell/lastdir"

    # elpa packages
    "/elpa/"

    # reftex files
    "*.rel"

    # AUCTeX auto folder
    "/auto/"

    # cask packages
    ".cask/"
    "dist/"

    # Flycheck
    "flycheck_*.el"

    # server auth directory
    "/server/"

    # projectiles files
    ".projectile"

    # directory configuration
    ".dir-locals.el"

    # network security
    "/network-security.data"
  ];
}
