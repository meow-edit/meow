(package-initialize)

(require 'paredit)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'meow)

(toggle-debug-on-error)

(meow-global-mode 1)

(setq-default mode-line-format
              '((:eval (meow-indicator))))

(define-key global-map (kbd "C-o")
  (lambda ()
    (interactive)
    (find-file "example.el")))
