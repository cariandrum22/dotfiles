;; -*- lexical-binding: t -*-

;;-----------------------------------------------------------------------------
;; system
;;-----------------------------------------------------------------------------
(setq inhibit-startup-message t)

(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (or (equal system-type 'usg-unix-v)
          (or (equal system-type 'berkeley-unix)
              (equal system-type 'cygwin)))))
(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin
  (equal system-type 'cygwin))
(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))
(defvar run-darwin (equal system-type 'darwin))

;;-----------------------------------------------------------------------------
;; language and encode setting
;;-----------------------------------------------------------------------------
(set-language-environment 'Japanese)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;;-----------------------------------------------------------------------------
;; transparency
;; from Tranceparent Emacs:emacs-fu
;;   http://emacs-fu.blogspot.jp/2009/02/transparent-emacs.html
;;-----------------------------------------------------------------------------
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
   decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;;-----------------------------------------------------------------------------
;; font
;;-----------------------------------------------------------------------------
(when run-w32
  (create-fontset-from-ascii-font "-outline-Ricty-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1" nil "Ricty")
  (set-fontset-font "fontset-Ricty"
                    'japanese-jisx0208
                    '("Ricty*" . "jisx0208-sjis"))
  (set-fontset-font "fontset-Ricty"
                    'katakana-jisx0201
                    '("Ricty*" . "jisx0201-katakana"))
  (set-face-attribute 'default nil :height 140))

;;-----------------------------------------------------------------------------
;; load configs for specific features and modes
;;-----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; install packages
(setq el-get-sources
      '(swiper
        projectile
        counsel-projectile
        use-package
        flycheck
        company-mode
        mmm-mode
        haskell-mode
        company-ghc
        ruby-mode
        ruby-end
        inf-ruby
        rspec-mode
        robe-mode
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
        emacs-fish
        font-lock+
        ddskk
        ov
        powerline
        color-theme-zenburn
        (:name hindent :type elpa)
        (:name sidebar :pkgname "sebastiencs/sidebar.el" :type github)
        (:name wrap-region :pkgname "rejeep/wrap-region" :type github)
        ))
(require 'init-el-get)
(package-initialize)

;; activate packages
(require 'use-package)
(require 'init-powerline)
(require 'init-ivy)
(require 'init-flycheck)
(require 'init-sidebar)
(require 'init-mmm)
(require 'init-emacs-lisp)
(require 'init-haskell)
(require 'init-ruby)
(require 'init-shell)
(require 'init-docker)
(require 'init-web)
(require 'init-js2)
(require 'init-elm)
(require 'init-yaml)

;;-----------------------------------------------------------------------------
;; theme
;;-----------------------------------------------------------------------------
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(background-color . "unspecified-bg"))

;;-----------------------------------------------------------------------------
;; auto-save and backup
;;-----------------------------------------------------------------------------
;; auto-save
(setq auto-save-timeout 10)
(setq auto-save-interval 50)

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

;;-----------------------------------------------------------------------------
;; others
;;-----------------------------------------------------------------------------
(setq-default tab-width 2 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

(setq vc-follow-symlinks t)
(setq auto-revert-check-vc-info t)

(show-paren-mode)

(display-time)

(electric-pair-mode t)

;; linum-mode
(add-hook 'find-file-hook 'linum-mode)
(setq linum-format " %3d ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (flycheck hindent)))
 '(sidebar-icon-header-end (quote (powerline_left_hard_divider 1)))
 '(sidebar-icon-powerline (quote (powerline_left_hard_divider 0 -0.05 1.0)))
 '(sidebar-icons-modeline
   (quote
    (powerline_left_hard_divider powerline_right_hard_divider 0))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#4c4c4c" :foreground "#9fc59f"))))
 '(sidebar-primary-color ((t (:background "#3f3f3f" :foreground "#7f9f7f"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#999999")))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
