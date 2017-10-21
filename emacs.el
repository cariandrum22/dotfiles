;; -*- lexical-binding: t -*-

;;-----------------------------------------------------------------------------
;; system
;;-----------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
      '(counsel-projectile
        use-package
        flycheck
        company-mode
        haskell-mode
        company-ghc
        rust-mode
        go-mode
        color-theme-zenburn
        (:name hindent :type elpa)
        (:name wrap-region :pkgname "rejeep/wrap-region" :type github)
        (:name telephone-line :pkgname "dbordak/telephone-line" :type github)
        ))
(require 'init-el-get)

;; activate packages
(require 'use-package)
(require 'init-telephone-line)
(require 'init-global)
(require 'init-flycheck)
(require 'init-emacs-lisp)
(require 'init-haskell)

;;-----------------------------------------------------------------------------
;; theme
;;-----------------------------------------------------------------------------
(load-theme 'zenburn t)

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

(show-paren-mode)
(display-time)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
