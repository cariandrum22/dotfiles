;;; init-web.el --- Web mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for web development (HTML, CSS, PHP, etc.).
;;; Code:

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
;;; init-web.el ends here
