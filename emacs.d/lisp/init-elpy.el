;;; init-elpy.el --- Elpy Python IDE configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Elpy Python development environment.
;;; Code:

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(provide 'init-elpy)
;;; init-elpy.el ends here
