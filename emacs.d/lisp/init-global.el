(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (global-company-mode)
                             (global-flycheck-mode)))

(provide 'init-global)
