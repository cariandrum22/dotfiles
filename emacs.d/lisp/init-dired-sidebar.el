;;; init-dired-sidebar.el --- Dired sidebar configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Dired sidebar with icons.
;;; Code:

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'icons
        dired-sidebar-use-term-integration t
        dired-sidebar-use-custom-font t))

(provide 'init-dired-sidebar)
;;; init-dired-sidebar.el ends here
