;;; init-docker.el --- Dockerfile mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Dockerfile editing with LSP support.
;;; Code:

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :hook (dockerfile-mode . lsp-deferred))

(provide 'init-docker)
;;; init-docker.el ends here
