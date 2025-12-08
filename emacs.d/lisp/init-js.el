;;; init-js.el --- JavaScript mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for JavaScript/JSX development with LSP support.
;;; Code:

(use-package js-mode
  :mode
  ("\\.js\\'"
    "\\.jsx\\'")
  :hook (js-mode . lsp-deferred))

(provide 'init-js)
;;; init-js.el ends here
