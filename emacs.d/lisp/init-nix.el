;;; init-nix.el --- Nix mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Nix expression editing with LSP support.
;;; Code:

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp-deferred))

(provide 'init-nix)
;;; init-nix.el ends here
