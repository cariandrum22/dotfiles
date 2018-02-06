(use-package skk
  :config
  (setq skk-tut-file "~/.emacs.d/el-get/ddskk/etc/SKK.tut")
  (setq skk-jisyo-code 'utf-8)
  ;; Integrate with AquaSKK
  (setq skk-server-host "127.0.0.1")
  (setq skk-server-portnum 1178)
  )

(provide 'init-skk)
