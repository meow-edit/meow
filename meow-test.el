(package-initialize)

(require 'paredit)
(require 'smartparens)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'meow)
(require 'meow-tut)

(toggle-debug-on-error)

(meow-global-mode 1)

(setq-default mode-line-format
              `(" "
                (:eval (meow-indicator))
                ,mode-line-format))

(meow-tut)
