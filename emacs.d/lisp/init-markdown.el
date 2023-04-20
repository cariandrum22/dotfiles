(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("/\\.git/COMMIT_EDITMSG\\'" . gfm-mode))
  :hook (markdown-mode . lsp-deferred)
  :init (setq markdown-command "pandoc"
              lsp-markdown-server-command "remark-language-server"
              lsp-markdown-server-command-args "--stdio"))

(provide 'init-markdown)
