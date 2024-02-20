;; -*- lexical-binding: t -*-

;;-----------------------------------------------------------------------------
;; system
;;-----------------------------------------------------------------------------
(setq inhibit-startup-message t)

;;-----------------------------------------------------------------------------
;; language and encode setting
;;-----------------------------------------------------------------------------
(set-language-environment 'English)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;-----------------------------------------------------------------------------
;; font
;;-----------------------------------------------------------------------------
(when (display-graphic-p)
      (set-frame-font "Fira Code-8" nil t)
      (set-fontset-font (frame-parameter nil 'font)
            'japanese-jisx0208
            (font-spec :family "Source Han Code JP-")))

;;-----------------------------------------------------------------------------
;; load configs for specific features and modes
;;-----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'use-package)
(require 'init-ivy)
(require 'init-powerline)
(require 'init-skk)
(require 'init-git)
(require 'init-which-key)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-haskell)
(require 'init-company)
(require 'init-company-shell)
(require 'init-docker)
(require 'init-fish)
(require 'init-js)
(require 'init-markdown)
(require 'init-mmm)
(require 'init-nix)
(require 'init-ruby)
(require 'init-shell)
(require 'init-toml)
(require 'init-yaml)
(require 'init-web)
(require 'init-whitespace)
(require 'init-ws-butler)
(require 'init-dired-sidebar)

;;-----------------------------------------------------------------------------
;; theme
;;-----------------------------------------------------------------------------
(load-theme 'nord t)

;;-----------------------------------------------------------------------------
;; auto-save and backup
;;-----------------------------------------------------------------------------
;; auto-save
(setq auto-save-timeout 10
      auto-save-interval 50)

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      version-control t
      kept-new-versions 5
      kept-old-versions 1
      delete-old-versions t)

;;-----------------------------------------------------------------------------
;; others
;;-----------------------------------------------------------------------------
(setq-default
      tab-width 2
      indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(setq vc-follow-symlinks t
      auto-revert-check-vc-info t)

(display-time-mode t)

(electric-pair-mode t)
(show-paren-mode t)
(wrap-region-mode t)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
