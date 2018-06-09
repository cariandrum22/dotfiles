(defun my-irony-mode-hook ()
  (define-key irony-mode-map
    [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map
    [remap complete-symbol]
    'irony-completion-at-point-async)
  )

(use-package irony
  :config
  ;(setq irony-server-install-prefix "where_to_install_irony")
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-to-list 'company-backends 'company-irony))

(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c++-mode-hook      'irony-mode)

(provide 'init-irony)
