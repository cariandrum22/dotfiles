(use-package js-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :hook (js-mode . lsp-deferred))

(provide 'init-js)
