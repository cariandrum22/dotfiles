;;; init-whitespace.el --- Whitespace visualization -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for visualizing whitespace characters.
;;; Code:

(use-package whitespace
  :init
  (setq whitespace-style
        '(face
          trailing
          tabs
          spaces
          lines
          lines-tail
          newline
          empty
          indentation
          space-after-tab
          space-before-tab
          big-indent
          space-mark
          tab-mark
          lines-tail))
  :config
  (global-whitespace-mode t))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
