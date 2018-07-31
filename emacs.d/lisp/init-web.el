(use-package web-mode
  :mode
  ("\\.p?html?\\'"
   "\\.erb\\'"
   "\\.jinja\\'"
   "\\.css\\'"
   "\\.php\\'"
   "\\.[agj]sp\\'")
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2))))

(provide 'init-web)
