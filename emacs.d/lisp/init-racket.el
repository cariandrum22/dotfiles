;;; init-racket.el --- Racket mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Racket development.
;;; Code:

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook
  ((racket-mode . racket-xp-mode))
  :bind
  (:map racket-mode-map
    ("C-c C-c" . racket-run)
    ("C-c C-t" . racket-test)
    ("C-c C-d" . racket-xp-describe)
    ("C-c C-." . racket-xp-visit-definition)
    ("C-c C-," . racket-unvisit))
  :config
  (setq racket-program "racket"))

(provide 'init-racket)
;;; init-racket.el ends here
