(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (global-whitespace-mode)
                             (global-company-mode)
                             (global-flycheck-mode)))

(provide 'init-global)
