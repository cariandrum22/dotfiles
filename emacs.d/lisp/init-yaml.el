(use-package yaml-mode
  :mode ("\\.y?ml\\'"
         "\\.sls\\'")
  :hook (yaml-mode . lsp-deferred))

(provide 'init-yaml)
