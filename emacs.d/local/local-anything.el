;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Original Author: rubikitch
; copy from http://www.emacswiki.org/emacs-en/anything-startup.el

(provide 'local-anything)

(require 'anything-config)
(require 'anything-match-plugin)

(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))

(when (require 'anything-complete nil t)
  (anything-lisp-complete-symbol-set-timer 150)
  (define-key emacs-lisp-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  (define-key lisp-interaction-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  (anything-read-string-mode 1)
  )

(require 'anything-show-completion)
(require 'anything-auto-install nil t)

(when (require 'descbinds-anything nil t)
  (descbinds-anything-install)
  )

(require 'anything-grep nil t)