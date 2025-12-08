;;; init-ruby.el --- Ruby mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Ruby development with LSP support.
;;; Code:

(use-package ruby-mode
  :mode
  ("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
    "\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'")
  :hook (ruby-mode . lsp-deferred))

(provide 'init-ruby)
;;; init-ruby.el ends here
