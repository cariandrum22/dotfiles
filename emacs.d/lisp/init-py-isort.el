(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save))

(provide 'init-py-isort)
