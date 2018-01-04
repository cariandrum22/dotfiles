(use-package flycheck
  :init
  (global-flycheck-mode)

  :config
  (setq flycheck-emacs-lisp-load-path 'load-path
        flycheck-display-errors-delay 0.3))

(provide 'init-flycheck)
