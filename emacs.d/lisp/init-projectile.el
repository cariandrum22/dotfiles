;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Projectile project management.
;;; Code:

(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
    ("s-p" . projectile-command-map)
    ("C-c p" . projectile-command-map)))

(provide 'init-projectile)
;;; init-projectile.el ends here
