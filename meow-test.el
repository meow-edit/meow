(package-initialize)

(require 'meow)

(setq meow-layout 'dvp)
(setq-default mode-line-format
              `(" "
                (:eval (meow-minimal-indicator))
                ,mode-line-format))
(load-theme 'leuven t)

(meow-global-mode)

;; (eval-buffer)
