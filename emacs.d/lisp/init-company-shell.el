;;; init-company-shell.el --- Company shell backend -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Company shell completion backend.
;;; Code:

(use-package company-shell
  :config
  (push 'company-shell company-backends))

(provide 'init-company-shell)
;;; init-company-shell.el ends here
