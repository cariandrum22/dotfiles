;;; init-yaml.el --- YAML mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for YAML files with LSP support.
;;; Code:

(use-package yaml-mode
  :mode
  ("\\.y?ml\\'"
    "\\.sls\\'")
  :hook (yaml-mode . lsp-deferred))

(provide 'init-yaml)
;;; init-yaml.el ends here
