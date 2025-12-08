;;; init-haskell.el --- Haskell mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Haskell development with LSP support.
;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook
  ((haskell-mode . lsp-deferred)
    (haskell-literate-mode . lsp-deferred))
  :bind
  (:map haskell-mode-map
    ("C-," . haskell-move-nested-left)
    ("C-." . haskell-move-nested-right)
    ("C-c C-." . haskell-mode-format-imports)
    ("s-i" . haskell-navigate-imports)
    ("C-c C-l" . haskell-process-load-or-reload)
    ("C-`" . haskell-interactive-bring)
    ("C-c C-t" . haskell-process-do-type)
    ("C-c C-i" . haskell-process-do-info)
    ("C-c C-c" . haskell-process-cabal-build)
    ("C-c C-k" . haskell-interactive-mode-clear)
    ("C-c c" . haskell-process-cabal)
    ("SPC" . haskell-mode-contextual-space))
  :config
  (setq haskell-process-log t))

(provide 'init-haskell)
;;; init-haskell.el ends here
