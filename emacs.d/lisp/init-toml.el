;;; init-toml.el --- TOML mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for TOML file editing with LSP support.
;;; Code:

(use-package toml-mode
  :mode "\\.toml\\'"
  :hook (toml-mode . lsp-deferred))

(provide 'init-toml)
;;; init-toml.el ends here
