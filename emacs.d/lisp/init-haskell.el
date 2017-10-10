(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (interactive)
                                 (subword-mode)
                                 (wrap-region-mode)
                                 (electric-pair-mode)
                                 (haskell-doc-mode)
                                 (interactive-haskell-mode)
                                 (haskell-indentation-mode)
                                 (ghc-init)
                                 (add-to-list 'company-backends 'company-ghc)))

  :config
  (setq haskell-process-suggest-remove-import-lines  t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-stylish-on-save t
        haskell-process-type 'stack-ghci)
  (bind-keys :map haskell-mode-map
             ("C-,"     . haskell-move-nested-left)
             ("C-."     . haskell-move-nested-right)
             ("C-c C-." . haskell-mode-format-imports)

             ("s-i"     . haskell-navigate-imports)

             ("C-c C-l" . haskell-process-load-or-reload)
             ("C-`"     . haskell-interactive-bring)
             ("C-c C-t" . haskell-process-do-type)
             ("C-c C-i" . haskell-process-do-info)
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c C-k" . haskell-interactive-mode-clear)
             ("C-c c"   . haskell-process-cabal)
             ;;("SPC"     . haskell-mode-contextual-space)
             ))

(provide 'init-haskell)
