

(defun helix-add-cursor-below ()
  "In helix, the commands bound by default to C"
  (interactive)
  (warn "unimplemented")
  ;; (mc/create-fake-cursor-at-point)
  ;; (next-line)
  ;; (multiple-cursors-mode)

  )

;; Nicked from kakoune.el
(defun helix-split-selection (beg end search)
  "Split region each time SEARCH occurs between BEG and END.

This can be thought of as an inverse to `mc/mark-all-in-region'."
  (interactive "r\nsSplit on: ")
  (let ((case-fold-search nil))
    (if (string= search "")
        (user-error "Empty search term")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (push-mark beg)
        (while (search-forward search end t)
          (save-excursion
            (goto-char (match-beginning 0))
            (mc/create-fake-cursor-at-point))
          (push-mark (match-end 0)))
        (unless (= (point) end)
          (goto-char end))
        (mc/maybe-multiple-cursors-mode)))))

(defun helix-select-regex ()
  (interactive)
  (warn "unimplemented")

;; (defun helix-split-selection-on-newline ())
;; (defun helix-merge-selections ())
;; (defun helix-merge-consecutive-selections
;; (defun helix-align-selections ())
;; (defun helix-trim-selections ())
;; (defun helix-collapse-selection ())
;; (defun helix-flip-selections ())
;; (defun helix-ensure-selections-forward ())
;; (defun helix-keep-primary-selection ())
;; (defun helix-remove-primary-selection ())
;; (defun helix-copy-selection-on-next-line ())
;; (defun helix-copy-selection-on-previous-line ())
;; (defun helix-rotate-selections-backward ())
;; (defun helix-rotate-selections-forward ())
;; (defun helix-rotate-selection-contents-backward ())
;; (defun helix-rotate-selection-contents-forward ())
;; (defun helix-extend-to-line-bounds ())
;; (defun helix-shrink-to-line-bounds ())
;; (defun helix-join-selections ())
;; (defun helix-join-selections-space ())
;; (defun helix-keep-selections ())
;; (defun helix-remove-selections ())
;; (defun helix-toggle-comments ())
  
(provide 'helix-selections)

;; treesitter
;;  expand-selection
