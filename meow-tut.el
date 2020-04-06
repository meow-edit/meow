;;; meow-tut.el --- Tutorial of Meow

(defconst meow--tutorial-buffer-name "*Meow Tutorial*")

(defconst meow--root (file-name-directory (or load-file-name buffer-file-name)))

(defun meow-tutorial ()
  (interactive)
  (with-current-buffer (get-buffer-create meow--tutorial-buffer-name)
    (erase-buffer)
    (insert-file-contents (expand-file-name "tutorial" meow--root))
    (goto-char (point-min))
    (text-mode))
  (switch-to-buffer meow--tutorial-buffer-name))

(provide 'meow-tut)
;;; meow-tut.el ends here
