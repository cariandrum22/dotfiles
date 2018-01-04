(use-package sidebar
  :init
  (use-package icons-in-terminal
    :load-path
    "~/.local/share/icons-in-terminal/"
    )
  
  :bind
  (("C-x C-f" . sidebar-open)
   ("C-x C-a" . sidebar-buffers-open)))

(provide 'init-sidebar)
