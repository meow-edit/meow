;;; meow-eldoc.el --- Make meow play well with eldoc

(defconst meow--eldoc-commands
  '(meow-head
    meow-tail
    meow-prev
    meow-next
    meow-exp
    meow-word
    meow-backward-word))

(defun meow--eldoc-setup ()
  "Setup commands those trigger eldoc.
Basically, all navigation commands should trigger eldoc."
  (apply #'eldoc-add-command meow--eldoc-commands))


(provide 'meow-eldoc)
;;; meow-eldoc.el ends here
