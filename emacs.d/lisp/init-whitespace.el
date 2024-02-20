(use-package whitespace
  :init (setq whitespace-style '(face
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
                                 lines-tail
                                 ))
  :config (global-whitespace-mode t))

(provide 'init-whitespace)
