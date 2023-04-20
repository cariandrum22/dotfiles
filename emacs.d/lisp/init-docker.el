(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :hook (dockerfile-mode . lsp-deferred))

(provide 'init-docker)
