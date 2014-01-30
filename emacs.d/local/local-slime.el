;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local Superior Lisp Interaction Mode for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'local-slime)
(setq inferior-lisp-program "/usr/local/bin/ccl")
(add-to-list 'load-path (expand-file-name "~ / .emacs.d / slime"))
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
