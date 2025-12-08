;;; init-git.el --- Git integration configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Magit and git-gutter.
;;; Code:

(use-package magit
  :bind
  (:map magit-mode-map
    ("C-x g" . magit-status))
  :config
  (global-git-gutter-mode))

(provide 'init-git)
;;; init-git.el ends here
