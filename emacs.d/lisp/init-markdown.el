;;; init-markdown.el --- Markdown mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Markdown editing with GitHub Flavored Markdown support.
;;; Code:

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
    ("/\\.git/COMMIT_EDITMSG\\'" . gfm-mode))
  :hook (markdown-mode . lsp-deferred)
  :init
  (setq markdown-command "pandoc"
    lsp-markdown-server-command "remark-language-server"
    lsp-markdown-server-command-args "--stdio"))

(provide 'init-markdown)
;;; init-markdown.el ends here
