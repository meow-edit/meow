;;; meow-tut.el --- Tutorial of Meow

(defconst meow--tut-buffer-name "*Meow Tutorial*")

(defun meow-tut ()
  (interactive)
  (with-current-buffer (get-buffer-create meow--tut-buffer-name)
    (erase-buffer)
    (insert-file-contents "tutorial")
    (goto-char (point-min))
    (text-mode))
  (switch-to-buffer meow--tut-buffer-name))

(provide 'meow-tut)
;;; meow-tut.el ends here
