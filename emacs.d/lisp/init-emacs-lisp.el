(defun my-emacs-lisp-setup ()
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'flycheck-mode-hook 'my-emacs-lisp-setup)

(provide 'init-emacs-lisp)
