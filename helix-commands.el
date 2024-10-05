(require 'evil-common)
(require 'evil-core)

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

(defun helix-move-next-word-start ()
  (interactive)
  (if (region-active-p)
      (progn
	(set-mark (max (point) (mark)))
	(evil-forward-beginning 'evil-word))
    (set-mark (point))
    (evil-forward-beginning 'evil-word)))

(defun helix-move-next-long-word-start ()
  (interactive)
  (if (region-active-p)
      (progn
	(set-mark (max (point) (mark)))
	(evil-forward-beginning 'evil-WORD))
    (set-mark (point))
    (evil-forward-beginning 'evil-WORD)))

(defun helix-move-next-word-end ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
  (let ((momentum (and (use-region-p) (< (mark) (point)))))
    (set-mark (point))
    (evil-forward-end 'evil-word 1)
    (when momentum
      (set-mark (+ 1 (mark))))
    (forward-char)))

(defun helix-move-next-long-word-end ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
  (let ((momentum (and (use-region-p) (< (mark) (point)))))
    (set-mark (point))
    (evil-forward-end 'evil-WORD 1)
    (when momentum
      (set-mark (+ 1 (mark))))
    (forward-char)))

(defun helix-move-prev-word-start ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
    (when (and (use-region-p) (< (mark) (point)))
      (forward-char))
  (set-mark (point))
  (evil-backward-beginning 'evil-word 1))

(defun helix-move-prev-long-word-start ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
    (when (and (use-region-p) (< (mark) (point)))
      (forward-char))
  (set-mark (point))
  (evil-backward-beginning 'evil-WORD 1))

(defun helix-mark-line ()
  (interactive)
  (set-mark (line-beginning-position))
  (goto-char (line-end-position)))

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

(provide 'helix-commands)
