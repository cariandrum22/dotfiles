;;; init-lsp.el --- LSP mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Language Server Protocol support.
;;; Code:

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-capf t
        lsp-headerline-breadcrumb-mod t))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
