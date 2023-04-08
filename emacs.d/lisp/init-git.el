(use-package magit
  :bind (:map magit-mode-map
              ("C-x g" . magit-status))
  :config (global-git-gutter-mode))

(provide 'init-git)
