{pkgs, ...}:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = (epkgs:
      (with epkgs; [
        swiper
        projectile
        counsel-projectile
        use-package
        flycheck
        company
        mmm-mode
        haskell-mode
        company-ghc
        enh-ruby-mode
        ruby-end
        inf-ruby
        rspec-mode
        robe
        rbenv
        projectile-rails
        rust-mode
        go-mode
        dockerfile-mode
        yaml-mode
        jinja2-mode
        web-mode
        js2-mode
        elm-mode
        fish-mode
        font-lock-plus
        magit
        git-gutter
        ddskk
        ov
        powerline
        zenburn-theme
        frame-local
        dired-sidebar
        wrap-region
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
}
