(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0)
  (mmm-add-mode-ext-class nil "\\.sls\\'" 'salt-mode)
  (mmm-add-classes
   '((salt-mode
      :submode jinja2-mode
      :front "{[\\%{]"
      :back "[\\%}]}")))
)

(provide 'init-mmm)
