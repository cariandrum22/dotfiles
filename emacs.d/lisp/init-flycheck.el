(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.3)
  (flycheck-define-checker python
    "A Python syntax and style checker using Ruff.

Ruff is a fast and lightweight Python linter and formatter that supports a wide range of linting rules. It was chosen for its performance and compatibility with modern Python projects.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "check"
              "--output-format=concise"
              (eval (when buffer-file-name (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :modes python-mode
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))))

(provide 'init-flycheck)
