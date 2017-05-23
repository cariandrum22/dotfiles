;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load personalized ellipses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/local")
(require 'local-cl)
(require 'local-setting)
(require 'local-package)
(require 'local-color-thema)
(require 'local-powerline)
(require 'local-cua-mode)
(require 'local-anything)
(require 'local-auto-complete)
(require 'local-yasnippet)
(require 'local-haskell-mode)
(require 'local-elixir-mode)
(require 'local-scala-mode2)
(require 'local-slime)
(require 'local-yaml-mode)
(require 'local-php-mode)
(require 'local-sf)
(require 'local-ecb)
(put 'upcase-region 'disabled nil)
