;;; meow-commands.el --- Commands in Meow
;;; -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; Implementation for all commands in Meow.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'seq)

(require 'meow-var)
(require 'meow-util)
(require 'meow-visual)
(require 'meow-bounds)
(require 'array)

(defun meow--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let ((cmd (key-binding (read-kbd-macro kbd-macro))))
    (call-interactively cmd)))

(defun meow--pop-selection ()
  "Pop a selection from variable `meow--selection-history' and activate."
  (when meow--selection-history
    (let ((sel (pop meow--selection-history)))
      (meow--select-without-history sel))))

(defun meow--make-selection (type mark pos &optional expand)
  "Make a selection with TYPE, MARK and POS.

The direction of selection is MARK -> POS."
  (if (and (region-active-p) expand)
      (let ((orig-mark (mark))
            (orig-pos (point)))
        (if (< mark pos)
            (list type (min orig-mark orig-pos) pos)
          (list type (max orig-mark orig-pos) pos)))
      (list type mark pos)))

(defun meow--select (selection &optional backward)
  "Mark the SELECTION."
  (unless (region-active-p)
    (meow--cancel-selection))
  (-let (((sel-type mark pos) selection))
    (if meow--selection
        (unless (equal meow--selection (car meow--selection-history))
          (push meow--selection meow--selection-history))
      ;; Used to restore the position where we starting selection
      (push (meow--make-selection nil (point) (point))
            meow--selection-history))
    (goto-char (if backward mark pos))
    (when sel-type
      (push-mark (if backward pos mark) t t)
      (setq meow--selection selection)))
  (force-mode-line-update))

(defun meow--select-without-history (selection)
  "Mark the SELECTION without record it in `meow--selection-history'."
  (-let (((sel-type mark point) selection))
    (goto-char point)
    (if (not sel-type)
        (progn
          (deactivate-mark)
          (message "No previous selection.")
          (meow--cancel-selection))
      (push-mark mark t t)
      (setq meow--selection selection))))

(defun meow--cancel-selection ()
  "Cancel current selection, clear selection history and deactivate the mark."
  (setq meow--selection-history nil
        meow--selection nil)
  (deactivate-mark t))

(defun meow-undo ()
  "Undo selection or buffer change.

Normal undo when there's no selection, otherwise undo the selection."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-undo))

(defun meow-pop-selection ()
  (interactive)
  (meow--pop-selection)
  (when (and (region-active-p) meow--expand-nav-function)
    (meow--highlight-num-positions)))

(defun meow-pop-all-selection ()
  (interactive)
  (while (meow--pop-selection)))

;;; exchange mark and point

(defun meow-reverse ()
  "Just exchange point and mark."
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)
    (meow--highlight-num-positions)))

;;; Buffer

(defun meow-begin-of-buffer ()
  "Mark from current point, to the beginning of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-min))
      (meow--select)))

(defun meow-end-of-buffer ()
  "Mark from current point, to the end of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-max))
      (meow--select)))

(defun meow-find-ref ()
  "Xref find."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-find-ref))

(defun meow-pop-marker ()
  "Pop marker."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-pop-marker))

;;; Clipboards

(defun meow-save ()
  "Copy, like command `kill-ring-save'."
  (interactive)
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))
    (when (equal '(expand . line) (meow--selection-type))
      (meow--add-newline-to-recent-kill-ring))))

(defun meow-yank ()
  "Yank."
  (interactive)
  (if-let ((yank-text (car kill-ring)))
    (let ((yank-to-newline (string-suffix-p "\n" yank-text)))
      (when yank-to-newline
        (goto-char (line-beginning-position)))
      (meow--execute-kbd-macro meow--kbd-yank))
    (meow--execute-kbd-macro meow--kbd-yank)))

(defun meow-yank-after ()
  (interactive)
  (when-let ((yank-text (car kill-ring)))
    (let ((yank-to-newline (string-suffix-p "\n" yank-text)))
      (if (not yank-to-newline)
          (save-mark-and-excursion
            (forward-char 1)
            (insert (car kill-ring)))
        (save-mark-and-excursion
          (goto-char (line-end-position))
          (if (= (point) (point-max))
              (newline)
            (forward-char 1))
          (insert (car kill-ring)))))))

(defun meow-yank-pop ()
  "Pop yank."
  (interactive)
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-yank-pop)))

;;; Quit

(defun meow-keyboard-quit ()
  "Keyboard quit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (meow--execute-kbd-macro meow--kbd-keyboard-quit)))

(defun meow-quit ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
    (delete-window)
    (previous-buffer)))

;;; Comment

(defun meow-comment ()
  "Comment region or comment line."
  (interactive)
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-comment)))

;;; Delete Operations

(defun meow-kill ()
  "Kill region or kill line."
  (interactive)
  (when (meow--allow-modify-p)
    (if (not (region-active-p))
        (meow--execute-kbd-macro meow--kbd-kill-line)
      (cond
       ((equal '(expand . line) (meow--selection-type))
        (when (and (not (meow--direction-backward-p))
                   (< (point) (point-max))
                   ;; we are not at the beginning of a non-empty line.
                   (not (and (= (point) (line-beginning-position))
                             (not (= (line-beginning-position) (line-end-position))))))
          (forward-char 1))
        (meow--execute-kbd-macro meow--kbd-kill-region))
       ((equal '(select . join) (meow--selection-type))
        (delete-indentation nil (region-beginning) (region-end)))
       (t (meow--execute-kbd-macro meow--kbd-kill-region))))))

(defun meow-kill-whole-line ()
  (interactive)
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-kill-whole-line)))

(defun meow-backward-delete ()
  "Backward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (call-interactively #'backward-delete-char)))

(defun meow-delete ()
  "Forward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (if (region-active-p)
        (delete-region (region-beginning) (region-end))
      (meow--execute-kbd-macro meow--kbd-delete-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE UP&DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-page-up ()
  "Page up."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-scoll-down))

(defun meow-page-down ()
  "Page down."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-scoll-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARENTHESIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-forward-slurp ()
  "Forward slurp sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-forward-slurp))

(defun meow-backward-slurp ()
  "Backward slurp sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-backward-slurp))

(defun meow-forward-barf ()
  "Forward barf sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-forward-barf))

(defun meow-backward-barf ()
  "Backward barf sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-backward-barf))

(defun meow-raise-sexp ()
  "Raise sexp."
  (interactive)
  (meow--cancel-selection)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (goto-char (car bounds))))
  (meow--execute-kbd-macro meow--kbd-raise-sexp))

(defun meow-transpose-sexp ()
  "Transpose sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-transpose-sexp))

(defun meow-split-sexp ()
  "Split sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-split-sexp))

(defun meow-join-sexp ()
  "Split sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-join-sexp))

(defun meow-splice-sexp ()
  "Splice sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-splice-sexp))

(defun meow-wrap-round ()
  "Wrap round paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-round))

(defun meow-wrap-square ()
  "Wrap square paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-square))

(defun meow-wrap-curly ()
  "Wrap curly paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-curly))

(defun meow-wrap-string ()
  "Wrap string."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATE TOGGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-temp-normal ()
  "Switch to navigation-only NORMAL state."
  (interactive)
  (when (meow-motion-mode-p)
    (message "Enter temporary normal mode.")
    (setq meow--temp-normal t)
    (meow--switch-state 'normal)))

(defun meow-insert ()
  "Move to the begin of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode.")
        (meow--switch-state 'motion))
    (meow--direction-backward)
    (meow--switch-state 'insert)))

(defun meow-insert-at-begin ()
  "Move to the begin of line, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode.")
        (meow--switch-state 'motion))
    (goto-char (line-beginning-position))
    (meow--switch-state 'insert)))

(defun meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode.")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (unless (region-active-p)
      (forward-char 1))
    (meow--switch-state 'insert)))

(defun meow-append-at-end ()
  "Move to the end of line, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode.")
        (meow--switch-state 'motion))
    (goto-char (line-end-position))
    (meow--switch-state 'insert)))

(defun meow-open-above ()
  "Open a newline above and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode.")
        (meow--switch-state 'motion))
    (goto-char (line-beginning-position))
    (save-mark-and-excursion
      (insert "\n"))
    (indent-for-tab-command)
    (meow--switch-state 'insert)))

(defun meow-open-below ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (message "Quit temporary normal mode.")
      (meow--switch-state 'motion)
    (goto-char (line-end-position))
    (newline-and-indent)
    (meow--switch-state 'insert)))

(defun meow-change ()
  "Kill current selection and switch to INSERT state."
  (interactive)
  (when (meow--allow-modify-p)
    (if (region-active-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (meow--switch-state 'insert))
      (delete-region (point) (1+ (point)))
      (meow--switch-state 'insert))))

(defun meow-change-save ()
  (interactive)
  (when (and (meow--allow-modify-p) (region-active-p))
    (kill-region (region-beginning) (region-end))
    (when (equal '(expand . line) (meow--selection-type))
      (meow--add-newline-to-recent-kill-ring))
    (meow--switch-state 'insert)))

(defun meow-replace ()
  "Replace current selection with yank."
  (interactive)
  (when (and (meow--allow-modify-p)
             (region-active-p))
    (delete-region (region-beginning) (region-end))
    (insert (string-trim-right (car kill-ring) "\n"))))

(defun meow-replace-save ()
  (interactive)
  (when (and (meow--allow-modify-p)
             (region-active-p))
    (-let ((type (meow--selection-type))
           ((beg . end) (car (region-bounds)))
           (yank-text (string-trim-right (car kill-ring) "\n")))
      (goto-char end)
      (insert yank-text)
      (goto-char end)
      (set-mark beg)
      (kill-region (region-beginning) (region-end))
      (goto-char (+ beg (length yank-text)))
      (when (equal '(expand . line) type)
        (meow--add-newline-to-recent-kill-ring)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHAR MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-head (arg)
  "Move towards to the head of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail (arg)
  "Move towards to the end of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

(defun meow-head-expand (arg)
  "Activate char selection, then move towards to the head of this line.

See `meow-head' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail-expand (arg)
  "Activate char selection, then move towards to the end of this line.

See `meow-tail' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

(defun meow-prev (arg)
  "Move to the previous line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the first line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (meow--execute-kbd-macro meow--kbd-backward-line))))))

(defun meow-next (arg)
  "Move to the next line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the last line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
	(meow--execute-kbd-macro meow--kbd-forward-line))))))

(defun meow-prev-expand (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'previous-line))))))

(defun meow-next-expand (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'next-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORD/SYMBOL MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun meow-mark-word (n)
;;   (interactive "p")
;;   (-let* ((num (* n (if (meow--direction-backward-p) -1 1)))
;;           ((beg . end)
;;            (save-mark-and-excursion
;;              (forward-word num)
;;              ;; This fix words in camelCase.
;;              (when (> num 0)
;;                (backward-char 1))
;;              (bounds-of-thing-at-point 'word))))
;;     (when (and beg end)
;;       (-> (meow--make-selection '(expand . word) beg end)
;;           (meow--select (< num 0))))))

(defun meow-mark-word (n)
  (interactive "p")
  (-let* (((beg . end) (bounds-of-thing-at-point 'word)))
    (when beg
      (-> (meow--make-selection '(expand . word) beg end)
          (meow--select (< n 0))))))

(defun meow-mark-symbol (n)
  (interactive "p")
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol)))
    (when beg
      (-> (meow--make-selection '(expand . word) beg end)
          (meow--select (< n 0))))))

(defun meow--forward-symbol-1 ()
  (forward-symbol 1))

(defun meow--backward-symbol-1 ()
  (forward-symbol -1))

(defun meow-next-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (meow--direction-forward)
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (forward-word n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
          (meow--select))
      (meow--highlight-num-positions '(backward-word . forward-word)))))

(defun meow-next-symbol (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (meow--direction-forward)
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (forward-symbol n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
          (meow--select))
      (meow--highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

(defun meow-back-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (meow--direction-backward)
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (backward-word n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
          (meow--select))
      (meow--highlight-num-positions '(backward-word . forward-word)))))

(defun meow-back-symbol (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (meow--direction-backward)
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (forward-symbol (- n))
              (unless (= (point) m)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
          (meow--select))
      (meow--highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINE SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--forward-line-1 ()
  (forward-line)
  (when meow--expanding-p
    (goto-char (line-end-position))))

(defun meow--backward-line-1 ()
  (forward-line -1))


(defun meow-line (n &optional expand)
  "Select the current line, eol is not included.

Create selection with type (expand . line).
For the selection with type (expand . line), expand it by line.
For the selection with other types, cancel it.

Prefix:
numeric, repeat times.
"
  (interactive "p")
  (unless (or expand (equal '(expand . line) (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((orig (mark))
         (n (if (meow--direction-backward-p)
               (- n)
             n))
         (forward (> n 0)))
    (cond
     ((region-active-p)
      (let (p)
        (save-mark-and-excursion
          (forward-line n)
          (goto-char
           (if forward
               (setq p (line-end-position))
             (setq p (line-beginning-position)))))
        (-> (meow--make-selection '(expand . line) orig p expand)
            (meow--select))
        (meow--highlight-num-positions '(previous-line . meow--forward-line-1))))
     (t
      (let ((m (if forward
                      (line-beginning-position)
                    (line-end-position)))
            (p (save-mark-and-excursion
                   (if forward
                       (progn
                       (forward-line (1- n))
                       (line-end-position))
                     (progn
                         (forward-line (1+ n))
                         (line-beginning-position))))))
        (-> (meow--make-selection '(expand . line) m p expand)
            (meow--select))
        (meow--highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1)))))))

(defun meow-line-expand (n)
  "Like `meow-line', but always expand."
  (interactive "p")
  (meow-line n t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--block-mark-list (arg &optional expand)
  (unless (equal '(select . block) (meow--selection-type))
    (meow--cancel-selection))
  (let* ((orig (point))
         (ra (and (region-active-p)
                  (equal '(select . block) (meow--selection-type))))
         (neg (xor (meow--direction-backward-p) (< (prefix-numeric-value arg) 0)))
         (search-fn (if neg #'re-search-backward #'re-search-forward))
         (m (if neg 1 2))
         (fix-pos (if neg 1 -1))
         beg end)
    (save-mark-and-excursion
      (when (or expand (not ra))
        (while (when (funcall search-fn "\\(\\s(\\)\\|\\(\\s)\\)" nil t)
                 (meow--in-string-p)))
        (when (match-string m)
          (forward-char fix-pos)))
      (-let ((bounds (bounds-of-thing-at-point 'list)))
        (setq beg (car bounds)
              end (cdr bounds))))
    (if (and beg end)
        (-> (meow--make-selection '(select . block)
                                  (if expand orig (if neg end beg))
                                  (if neg beg end)
                                  expand)
            (meow--select))
      (message "Mark block failed"))))

(defun meow-block (arg &optional expand)
  "Mark the block or expand to parent block."
  (interactive "P")
  (meow--block-mark-list arg expand))

(defun meow-block-expand (arg)
  "Expand to next block.

Will create selection with type (expand . block)."
  (interactive "P")
  (meow-block arg t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JOIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--join-forward ()
  (let (mark pos)
    (save-mark-and-excursion
      (goto-char (line-end-position))
      (setq pos (point))
      (when (re-search-forward "[[:space:]\n\r]*" nil t)
        (setq mark (point))))
    (when pos
      (-> (meow--make-selection '(select . join) pos mark)
          (meow--select)))))

(defun meow--join-backward ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (back-to-indentation)
      (setq pos (point))
      (goto-char (line-beginning-position))
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (-> (meow--make-selection '(select . join) mark pos)
        (meow--select))))

(defun meow--join-both ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (save-mark-and-excursion
      (while (looking-at "[[:space:]\n\r]")
        (forward-char 1))
      (setq pos (point)))
    (-> (meow--make-selection '(select . join) mark pos)
        (meow--select))))

(defun meow-join (arg)
  "Select the indentation between this line to the non empty previous line.

Will create selection with type (select . join)

Prefix:
with NEGATIVE ARGUMENT, forward search indentation to select.
with UNIVERSAL ARGUMENT, search both side."
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--join-both))
   ((meow--with-negative-argument-p arg)
    (meow--join-forward))
   (t
    (meow--join-backward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND & TILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--find-continue-forward ()
  (let ((ch-str (char-to-string (cdr (meow--selection-type)))))
    (search-forward ch-str nil t 1)))

(defun meow--find-continue-backward ()
  (let ((ch-str (char-to-string (cdr (meow--selection-type)))))
    (search-backward ch-str nil t 1)))

(defun meow--till-continue-forward ()
  (let ((ch-str (char-to-string (cdr (meow--selection-type)))))
    (forward-char 1)
    (when (search-forward ch-str nil t 1)
      (backward-char 1))))

(defun meow--till-continue-backward ()
  (let ((ch-str (char-to-string (cdr (meow--selection-type)))))
    (backward-char 1)
    (when (search-backward ch-str nil t 1)
      (forward-char 1))))

(defun meow-find (n &optional prompt expand)
  "Find the next N char read from minibuffer."
  (interactive "p")
  (let* ((ch (read-char (or prompt (message "Find(%d):" n))))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end)
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (-> (meow--make-selection (cons 'find ch) beg end expand)
          (meow--select))
      (meow--highlight-num-positions
       '(meow--find-continue-backward . meow--find-continue-forward)))))

(defun meow-find-expand (n)
  (interactive "p")
  (meow-find n (message "Expand find(%d):" n) t))

(defun meow-till (n &optional prompt expand)
  "Forward till the next N char read from minibuffer."
  (interactive "p")
  (let* ((ch (read-char (message "Till(%d):" n)))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (-> (meow--make-selection (cons 'find ch) beg (+ end fix-pos) expand)
          (meow--select))
      (meow--highlight-num-positions
       '(meow--till-continue-backward . meow--till-continue-forward)))))

(defun meow-till-expand (n)
  (interactive "p")
  (meow-till n (message "Expand till(%d):" n) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISIT and SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-search ()
  "Searching for the same text in selection or next visited text."
  (interactive)
  (when (and (not (equal '(select . visit) (meow--selection-type)))
             (region-active-p))
    (setq meow--last-search
          (buffer-substring-no-properties (region-beginning) (region-end))))
  (when meow--last-search
    (let ((reverse (meow--direction-backward-p))
          (case-fold-search nil)
          (search meow--last-search))
      (if search
          (if (if reverse
                  (re-search-backward search nil t 1)
                (re-search-forward search nil t 1))
              (-let* (((marker-beg marker-end) (match-data))
                      (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                      (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
                (-> (meow--make-selection '(select . visit) beg end)
                    (meow--select))
                (if reverse
                    (message "Reverse search: %s" search)
                  (message "Search: %s" search)))
            (message "Searching text not found"))
        (message "No search text"))
      (meow--highlight-regexp-in-buffer search))))

(defun meow--visit-point (text reverse)
  "Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
  (let ((func (if reverse #'re-search-backward #'re-search-forward))
        (func-2 (if reverse #'re-search-forward #'re-search-backward)))
    (save-mark-and-excursion
      (or (funcall func text nil t 1)
          (funcall func-2 text nil t 1)))))

(defun meow-visit (arg)
  "Mark the search text.
Argument ARG if not nil, reverse the selection when make selection."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (text (meow--prompt-symbol-and-words
                (if arg "Visit backward: " "Visit: ")
                (point-min) (point-max)))
         (visit-point (meow--visit-point text reverse)))
    (if visit-point
        (-let* (((marker-beg marker-end) (match-data))
                (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
                (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (-> (meow--make-selection '(select . visit) beg end)
              (meow--select))
          (setq meow--last-search text)
		  (meow--highlight-regexp-in-buffer text)
      (message "Searching text not found")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-beginning-of-thing (ch &optional expand)
  "Select to the beginning of thing represented by CH.
When EXPAND is non-nil, extend current selection.

Prefix argument is not allow for this command."
  (interactive "cBeginning of:")
  (let ((bounds (meow--parse-inner-of-thing-char ch)))
   (when bounds
     (-> (meow--make-selection '(select . transient)
                               (point)
                               (car bounds))
         (meow--select)))))

(defun meow-end-of-thing (ch &optional expand)
   "Select to the beginning of thing represented by CH.
When EXPAND is non-nil, extend current selection.

Prefix argument is not allow for this command."
  (interactive "cEnd of:")
  (let ((bounds (meow--parse-inner-of-thing-char ch)))
   (when bounds
     (-> (meow--make-selection '(select . transient)
                               (point)
                               (cdr bounds))
         (meow--select)))))

(defun meow-inner-of-thing (ch)
  (interactive "cInner of:")
  (let ((bounds (meow--parse-inner-of-thing-char ch)))
    (when bounds
      (-> (meow--make-selection '(select . transient)
                                (car bounds)
                                (cdr bounds))
          (meow--select)))))

(defun meow-bounds-of-thing (ch)
  (interactive "cBounds of:")
  (let ((bounds (meow--parse-bounds-of-thing-char ch)))
    (when bounds
      (-> (meow--make-selection '(select . transient)
                                (car bounds)
                                (cdr bounds))
          (meow--select)))))

(defun meow-indent ()
  "Indent region or current line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-indent-region))

(defun meow-M-x ()
  "Just Meta-x."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-excute-extended-command))

(defun meow-pop-to-mark ()
  (interactive)
  (when-let (m (car mark-ring))
    (goto-char m)))

(defun meow-back-to-indentation ()
  "Back to indentation."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-back-to-indentation))

(defun meow-query-replace (arg)
  "Query-replace.

Argument ARG ignored."
  (interactive "P")
  (if arg
      (meow--execute-kbd-macro meow--kbd-query-replace)
    (meow--execute-kbd-macro meow--kbd-query-replace-regexp)))

(defun meow-last-buffer (arg)
  "Switch to last buffer.
Argument ARG if not nil, switching in a new window."
  (interactive "P")
  (cond
   ((minibufferp)
    (keyboard-escape-quit))
   ((not arg)
    (mode-line-other-buffer))
   (t
    (split-window)
    (mode-line-other-buffer))))

(defun meow-escape-or-normal-modal ()
  "Keyboard escape quit or switch to normal state."
  (interactive)
  (cond
   ((minibufferp)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))
   ((eq major-mode 'fundamental-mode)
    (meow--switch-state 'normal))))

(defun meow-space ()
  "In MOTION state, the original command that bound to SPC will be executed.
In NORMAL state, execute the command on M-SPC(default to just-one-space)."
  (interactive)
  (if (and (meow-motion-mode-p) meow--space-command)
      (call-interactively meow--space-command)
    (meow--execute-kbd-macro meow--kbd-just-one-space)))

(defun meow-eval-last-exp ()
  "Eval last sexp."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-eval-last-exp))

(defun meow-expand (&optional n)
  (interactive)
  (when meow--highlight-overlays
    (let* ((n (or n (string-to-number (char-to-string last-input-event))))
           (n (if (= n 0) 10 n))
           (sel-type (meow--selection-type))
           (sel-type (cons 'expand (cdr sel-type))))
      (-> (meow--make-selection sel-type (mark)
                                (save-mark-and-excursion
                                  (let ((meow--expanding-p t))
                                    (dotimes (_ n)
                                      (funcall
                                       (if (meow--direction-backward-p)
                                           (car meow--expand-nav-function)
                                         (cdr meow--expand-nav-function)))))
                                  (point)))
          (meow--select))
      (meow--highlight-num-positions meow--expand-nav-function))))

(defun meow-expand-1 () (interactive) (meow-expand 1))
(defun meow-expand-2 () (interactive) (meow-expand 2))
(defun meow-expand-3 () (interactive) (meow-expand 3))
(defun meow-expand-4 () (interactive) (meow-expand 4))
(defun meow-expand-5 () (interactive) (meow-expand 5))
(defun meow-expand-6 () (interactive) (meow-expand 6))
(defun meow-expand-7 () (interactive) (meow-expand 7))
(defun meow-expand-8 () (interactive) (meow-expand 8))
(defun meow-expand-9 () (interactive) (meow-expand 9))
(defun meow-expand-0 () (interactive) (meow-expand 0))

(provide 'meow-command)
;;; meow-command.el ends here
