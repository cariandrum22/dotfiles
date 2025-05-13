(use-package elpy
  :ensure t
  :init (elpy-enable)
  :hook (elpy-mode . flycheck-mode)
  :config (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(provide 'init-elpy)
