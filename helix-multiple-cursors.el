(require 'multiple-cursors)

(defun helix-add-cursor-below ()
  "In helix, the commands bound by default to C"
  (interactive)
  (mc/create-fake-cursor-at-point)
  (next-line)
  (multiple-cursors-mode))

(provide 'helix-multiple-cursors)
