(use-package toml-mode
  :mode "\\.toml\\'"
  :hook (toml-mode . lsp-deferred))

(provide 'init-toml)
