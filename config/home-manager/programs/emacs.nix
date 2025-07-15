{ pkgs, ... }:
let
  nord-theme' = pkgs.fetchFromGitHub {
    owner = "cariandrum22";
    repo = "emacs";
    rev = "fix-invalid-face-box-error";
    sha256 = "ZU5dGvGrZvivbA+981IyIu8a4xftL3QyUavGwBc7mQc=";
  };
  ws-butler' = pkgs.emacsPackages.trivialBuild {
    pname = "ws-butler";
    version = "20250310.205";
    src = pkgs.fetchgit {
      url = "https://github.com/lewang/ws-butler.git";
      rev = "e3a38d93e01014cd47bf5af4924459bd145fd7c4";
      sha256 = "sha256-P3G5iEmwK/mijvNRyrqxG4xypnbGh3r6SHOBqZMT89g=";
      leaveDotGit = true;
    };
  };
in
{
  nixpkgs.overlays = [
    (import (
      builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      }
    ))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = (
      epkgs:
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
        ws-butler'
        company
        company-shell
        lsp-mode
        lsp-ui
        lsp-ivy
        lsp-treemacs
        haskell-mode
        lsp-haskell
        proof-general
        elpy
        py-isort

        dockerfile-mode
        fish-mode
        jinja2-mode
        markdown-mode
        mmm-mode
        nix-mode
        toml-mode
        web-mode
        yaml-mode
        (melpaPackages.nord-theme.overrideAttrs (old: {
          src = nord-theme';
        }))
        all-the-icons
        all-the-icons-dired
        dired-sidebar
      ])
    );
  };

  home.file = {
    ".emacs.el" = {
      source = ../../../emacs.el;
    };
    ".emacs.d" = {
      source = ../../../emacs.d;
      recursive = true;
    };
  };

  programs.git.ignores = [
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
