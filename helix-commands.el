(require 'evil-common)
(require 'evil-core)

(defun helix-previous-line (&optional arg try-vscroll)
  (interactive "p")
  (deactivate-mark)
  (previous-line arg try-vscroll))

(defun helix-next-line (&optional arg try-vscroll)
  (interactive "p")
  (deactivate-mark)
  (next-line arg try-vscroll))

(defun helix-forward-char (&optional n)
  (interactive "p")
  (deactivate-mark)
  (forward-char n))

(defun helix-backward-char (&optional n)
  (interactive "p")
  (deactivate-mark)
  (backward-char n))


(defun helix-append ()
  (interactive)
  (if (region-active-p)
      (goto-char (max (point) (mark)))
       (forward-char))
  (evil-insert-state))

(defun helix-insert ()
  (interactive)
  (when (region-active-p)
    (goto-char (min (point) (mark))))
  (evil-insert-state))

(defun helix-move-next-word-start (n)
  (interactive "p")
  (dotimes (i n)
    (if (region-active-p)
	(progn
	  (set-mark (max (point) (mark)))
	  (evil-forward-beginning 'evil-word))
      (set-mark (point))
      (evil-forward-beginning 'evil-word))))

(defun helix-move-next-long-word-start (n)
  (interactive "p")
  (dotimes (i n)
    (if (region-active-p)
	(progn
	  (set-mark (max (point) (mark)))
	  (evil-forward-beginning 'evil-WORD))
      (set-mark (point))
      (evil-forward-beginning 'evil-WORD))))

(defun helix-move-next-word-end (n)
  (interactive "p")
  (dotimes (i n)
    (when (and (region-active-p) (> (point) (mark)))
      (backward-char))
    (let ((momentum (and (use-region-p) (< (mark) (point)))))
      (set-mark (point))
      (evil-forward-end 'evil-word 1)
      (when momentum
	(set-mark (+ 1 (mark))))
      (forward-char))))

(defun helix-move-next-long-word-end (n)
  (interactive "p")
  (dotimes (i n)
    (when (and (region-active-p) (> (point) (mark)))
      (backward-char))
    (let ((momentum (and (use-region-p) (< (mark) (point)))))
      (set-mark (point))
      (evil-forward-end 'evil-WORD 1)
      (when momentum
	(set-mark (+ 1 (mark))))
      (forward-char))))

(defun helix-move-previous-word-start (n)
  (interactive "p")
  (dotimes (i n)
    (when (and (region-active-p) (> (point) (mark)))
      (backward-char))
    (when (and (use-region-p) (< (mark) (point)))
      (forward-char))
    (set-mark (point))
    (evil-backward-beginning 'evil-word 1)))

(defun helix-move-previous-long-word-start (n)
  (interactive "p")
  (dotimes (i n)
    (when (and (region-active-p) (> (point) (mark)))
      (backward-char))
    (when (and (use-region-p) (< (mark) (point)))
      (forward-char))
    (set-mark (point))
    (evil-backward-beginning 'evil-WORD 1)))

(defun helix-extend-line-below (n)
  "Mark the current line. If already selected, extend to next line."
  (interactive "p")
  (dotimes (i n)
    (if (helix--current-line-selected-p)
	(progn
	  (set-mark (min (point) (mark)))
	  (next-line)
	  (goto-char (line-end-position)))
      (set-mark (line-beginning-position))
      (goto-char (line-end-position)))))

(defun helix--current-line-selected-p ()
  "Return t if the current line is selected (part of the active region)."
  (and (region-active-p)
       (and (<= (mark) (line-beginning-position))
	    (>= (point) (line-end-position)))))

(defun helix-delete-region-or-char ()
  (interactive)
  (if (not (region-active-p))
      (delete-char 1)
    (helix-prepare-region-for-kill)
    (kill-region (mark) (point))))

(defun helix-region-is-full-line-p ()
  "Return t if the region is a full line."
  (and (use-region-p)
       (= (line-beginning-position) (region-beginning))
       (= (line-end-position) (region-end))))

(defun helix-prepare-region-for-kill ()
  (when (and (helix-region-is-full-line-p)
             (< (point) (point-max)))
    (forward-char 1)))

(defun helix-collapse-region ()
  (interactive)
  (when (region-active-p)
    (if (> (point) (mark))
	(progn
	  (deactivate-mark)
	  (backward-char))
      (deactivate-mark))))

(defun helix-change ()
  (interactive)
  (if (not (region-active-p))
      (progn
	(delete-char 1)
	(evil-insert-state))
    (progn
      (kill-region (mark) (point))
      (evil-insert-state))))

(defun helix-find-char (n ch &optional expand)
  "Find the next N char read from minibuffer."
  (interactive "p\ncFind:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end)
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (set-mark (point))
      (goto-char end))
      (setq meow--last-find ch)
      ;; (meow--maybe-highlight-um-positions
      ;;  '(meow--find-continue-backward . meow--find-continue-forward))

      ))

(defun helix-kill-ring-save ()
  (interactive)
  (helix-prepare-region-for-kill)
  (kill-ring-save nil nil t))

(defun helix-open-above ()
  (interactive)
  (previous-line)
  (goto-char (line-end-position))
  (default-indent-new-line)
  (evil-insert-state))

(defun helix-open-below ()
  (interactive)
  (goto-char (line-end-position))
  (default-indent-new-line)
  (evil-insert-state))

(defun helix-toggle-kmacro-recording ()
  "Start or stop recording a keyboard macro."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

;; (defun jump-to-matching-bracket ()
;;   "Jump to the matching bracket if on a bracket character."
;;   (interactive)
;;   (let ((char (char-after)))
;;     (cond
;;      ((or (eq char ?\() (eq char ?\[) (eq char ?\{))
;;       (forward-sexp 1))
;;      ((or (eq char ?\)) (eq char ?\]) (eq char ?\}))
;;       (backward-sexp 1))
;;      (t
;;       (message "Not on a bracket character.")))))

(provide 'helix-commands)
