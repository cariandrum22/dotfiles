;;; init-mmm.el --- MMM mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Multiple Major Modes (e.g., Jinja2 in Salt files).
;;; Code:

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe
    mmm-submode-decoration-level 0)
  (mmm-add-mode-ext-class nil "\\.sls\\'" 'salt-mode)
  (mmm-add-classes
    '((salt-mode
        :submode jinja2-mode
        :front "{[\\%{]"
        :back "[\\%}]}"))))

(provide 'init-mmm)
;;; init-mmm.el ends here
