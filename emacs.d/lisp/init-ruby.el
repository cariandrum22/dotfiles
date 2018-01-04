(use-package ruby-mode
  :config
  (progn
    (use-package robe
      :config
      (eval-after-load 'company
        '(push 'company-robe company-backends)))
    )
  )

(provide 'init-ruby)
