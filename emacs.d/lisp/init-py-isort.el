;;; init-py-isort.el --- Python import sorting -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for automatic Python import sorting.
;;; Code:

(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save))

(provide 'init-py-isort)
;;; init-py-isort.el ends here
