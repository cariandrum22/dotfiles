(use-package skk
  :bind  (("C-x C-j" . skk-mode)
          ("C-x j"   . skk-auto-fill-mode))
  :config (setq skk-jisyo-code 'utf-8))

(provide 'init-skk)
