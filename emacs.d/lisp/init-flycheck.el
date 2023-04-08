(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.3))

(provide 'init-flycheck)
