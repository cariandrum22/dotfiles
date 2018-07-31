(use-package js2-mode
  :mode
  ("\\.js\\'"
   "\\.jsx\\'")
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2))))

(provide 'init-js2)
