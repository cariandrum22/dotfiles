;;; init-ws-butler.el --- Whitespace butler configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for automatic whitespace cleanup.
;;; Code:

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(provide 'init-ws-butler)
;;; init-ws-butler.el ends here
