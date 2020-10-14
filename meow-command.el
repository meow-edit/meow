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
  (-let (((sel-type point mark) selection))
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

(defun meow-pop-selection (arg)
  (interactive "P")
  (if (meow--with-universal-argument-p arg)
      (while (meow--pop-selection))
    (meow--pop-selection)))

;;; exchange mark and point

(defun meow-reverse ()
  "Just exchange point and mark."
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark))
  (force-mode-line-update))

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

(defun meow-last-pos ()
  (interactive)
  (meow--cancel-selection)
  (when meow--position-history
    (let ((pos (pop meow--position-history)))
      (goto-char pos))))

;;; Clipboards

(defun meow-copy ()
  "Copy, like command `kill-ring-save'."
  (interactive)
  (if (region-active-p)
      (if (eq 'line (meow--selection-type))
          (progn
            (when (and (not (meow--direction-backward-p))
                       (< (point) (point-max)))
              (forward-char 1))
            (meow--execute-kbd-macro meow--kbd-kill-ring-save))
        (meow--execute-kbd-macro meow--kbd-kill-ring-save))
    (meow--selection-fallback)))

(defun meow-yank (arg)
  "Yank.

Use universal argument for exchange yank.
Use negative argument for overwrite yank.
"
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (when (region-active-p)
      (let ((text (pop kill-ring)))
        (kill-region (region-beginning) (region-end))
        (insert text))))
   ((meow--with-negative-argument-p arg)
    (when (region-active-p)
      (delete-region (region-beginning) (region-end))
      (yank)))
   (t (meow--execute-kbd-macro meow--kbd-yank))))

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
    (meow--execute-kbd-macro meow--kbd-delete-char)))

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

(defun meow-open ()
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
    (if (not (region-active-p))
        (meow--selection-fallback)
      (meow--execute-kbd-macro meow--kbd-kill-region)
      (meow--switch-state 'insert))))

(defun meow-newline ()
  (interactive)
  (when (meow--allow-modify-p)
    (newline 2)
    (indent-for-tab-command)
    (forward-line -1)
    (indent-for-tab-command)
    (meow--switch-state 'insert)))

(defun meow-replace ()
  "Replace current selection with yank."
  (interactive)
  (when (meow--allow-modify-p)
    (if (not (region-active-p))
        (meow--selection-fallback)
      (delete-region (region-beginning) (region-end))
      (yank))))

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

;;; Line navigation/selection

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
        (call-interactively #'previous-line))))))

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
        (call-interactively #'next-line))))))

;;; line selections

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

;;; w
(defun meow-word (arg &optional expand)
  "Move word forward, select from the origin position to new position.

Create the selection with type (select . word).
Expand region by word if current selection has type (expand . word)

Prefix:
- numeric, repeat times.
- universal, move by group of words(to the edge of symbol)."
  (interactive "P")
  (let* ((mark (point))
         (expand (or expand
                     (equal '(mark . word) (meow--selection-type))
                     (equal '(expand . word) (meow--selection-type))))
         (pos (save-mark-and-excursion
                (when (and (region-active-p) expand)
                  (goto-char (region-end)))
                (cond ((meow--with-universal-argument-p arg)
                       (forward-symbol 1))
                      (t
                       (forward-word (prefix-numeric-value arg))))
                (point))))
    (-> (if expand
            (meow--make-selection '(expand . word) mark pos expand)
          (meow--make-selection '(select . word) mark pos))
        (meow--select))))

;;; W

(defun meow-word-expand (arg)
  "Like `meow-word' but create the selection with type (expand . word)."
  (interactive "P")
  (meow-word arg t))

;;; m
(defun meow-mark-word (arg &optional expand)
  "Select the word under cursor or the previous word if this word is already selected.
Create the selection with type (anchor . word)."
  (interactive "P")
  (let (bound)
    (save-mark-and-excursion
      (when (and (region-active-p) (equal '(mark . word) (meow--selection-type)))
        (backward-word 1))
      (unless (looking-back "\\b" 1)
        (backward-word 1))
      (while (and (> (point) (point-min)) (not bound))
        (setq bound
              (bounds-of-thing-at-point
               (if (meow--with-universal-argument-p arg) 'symbol 'word)))
        (backward-word 1)))
    (-> (meow--make-selection '(mark . word) (cdr bound) (car bound) expand)
        (meow--select))))

;;; M
(defun meow-mark-word-expand (arg)
  "Like `meow-mark-word-expand' but expand the region."
  (interactive "P")
  (meow-mark-word arg t))

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
  (let* ((n (if (meow--direction-backward-p)
               (- n)
             n))
        (forward (> n 0)))
    (cond
     ((region-active-p)
      (forward-line n)
      (goto-char
       (if forward
           (line-end-position)
         (line-beginning-position))))
     (t
      (let ((mark (if forward
                      (line-beginning-position)
                    (line-end-position)))
            (pos (save-mark-and-excursion
                   (if forward
                       (progn
                       (forward-line (1- n))
                       (line-end-position))
                     (progn
                         (forward-line (1+ n))
                         (line-beginning-position))))))
        (-> (meow--make-selection '(expand . line) mark pos expand)
            (meow--select)))))))

(defun meow-line-expand (n)
  "Like `meow-line', but always expand."
  (interactive "p")
  (meow-line n t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--block-mark-defun ()
  (-let (((beg . end) (bounds-of-thing-at-point 'defun)))
    (when beg
      (-> (meow--make-selection '(select . block) beg end)
          (meow--select)))))

(defun meow--block-mark-tag (&optional expand)
  (let ((rbeg (or (when (region-active-p) (region-beginning)) (point)))
        (rend (or (when (region-active-p) (region-end)) (point)))
        beg end err stack special-tag)
    (save-mark-and-excursion
      (when (if expand
                (re-search-forward "\\(?:<\\(/[a-zA-Z0-9]+\\)\\(?: .+\\)?>\\)" nil t)
              (re-search-forward "\\(?:<\\(/[a-zA-Z0-9]+\\)\\(?: .+\\)?>\\|\\(/>\\)\\|\\(%>\\)\\|\\(-->\\)\\)" nil t))
        (cond
         ((match-string 2)
          (setq end (point)
                beg (re-search-backward "<[a-zA-Z0-9]" nil t)
                special-tag t))
         ((match-string 3)
          (setq end (point)
                beg (re-search-backward "<%" nil t)
                special-tag t))
         ((match-string 4)
          (setq end (point)
                beg (search-backward "<!--" nil t)
                special-tag t))
         (t
          (let ((tag (meow--remove-text-properties (match-string 1))))
          (push tag stack)
          (setq end (point))
          (search-backward "<")
          (while (and stack (not err))
            (re-search-backward "<\\(/?[a-zA-Z0-9]+\\)\\(?: .+[^/%-]\\)?>" nil t)
            (let ((tag (meow--remove-text-properties (match-string 1))))
              (if (string-prefix-p "/" tag)
                  (push tag stack)
                (if (string-equal (concat "/" tag) (car stack))
                    (pop stack)
                  (setq err t)))))
          (setq beg (point)))))))
    (when err (message "Found unmatched tag!"))
    (if beg
        (if (or special-tag (not expand) (<= beg rbeg))
            (-> (meow--make-selection '(select . block) beg end)
                (meow--select))
          (goto-char end)
          (set-mark rbeg)
          (meow--block-mark-tag t))
      (message "Mark block failed"))))

(defun meow--block-mark-list (arg &optional expand)
  (let* ((orig (point))
         (ra (and (region-active-p)
                  (equal '(select . block) (meow--selection-type))))
         (backward (meow--direction-backward-p))
         (neg (< (prefix-numeric-value arg) 0))
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
            (meow--select backward))
      (message "Mark block failed"))))

(defun meow-block (arg &optional expand)
  "Mark the block or expand to parent block."
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--block-mark-defun))
   ((and (derived-mode-p 'web-mode)
         (not (eq nil (get-text-property (point) 'part-side))))
    (meow--block-mark-list arg expand))
   ((derived-mode-p 'web-mode 'html-mode 'mhtml-mode)
    (meow--block-mark-tag (region-active-p)))
   (t (meow--block-mark-list arg expand))))

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

(defun meow-till (arg &optional ch expand)
  ""
  (interactive "P")
  (let* ((ch (or ch (read-char "Till:")))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (n (prefix-numeric-value arg))
         (beg (if expand (mark) (point)))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if end
        (-> (meow--make-selection (cons 'find ch) beg (+ end fix-pos))
            (meow--select))
      (message "character %s not found" ch-str))))

(defun meow-till-expand (arg)
  ""
  (interactive "P")
  (when (equal (car (meow--selection-type)) 'find)
    (let ((ch (cdr (meow--selection-type))))
      (meow-till arg ch t))))

(defun meow-find (arg &optional ch expand)
  ""
  (interactive "P")
  (let* ((ch (or ch (read-char "Find:")))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (n (prefix-numeric-value arg))
         (beg (if expand (mark) (point)))
         end)
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if end
        (-> (meow--make-selection (cons 'find ch) beg end)
            (meow--select))
      (message "character %s not found" ch-str))))

(defun meow-find-expand (arg)
  ""
  (interactive "P")
  (when (equal (car (meow--selection-type)) 'find)
    (let ((ch (cdr (meow--selection-type))))
      (meow-find arg ch t))))

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
  (let ((reverse (meow--direction-backward-p))
        (search meow--last-search))
    (if search
        (if (if reverse
                (re-search-backward search nil t 1)
              (or (re-search-forward search nil t 1)
                  (when (re-search-backward search nil t 1)
                    (setq reverse t))))
            (-let* (((marker-beg marker-end) (match-data))
                    (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                    (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
              (-> (meow--make-selection '(select . visit) beg end)
                  (meow--select))
              (if reverse
                  (message "Reverse search: %s" search)
                (message "Search: %s" search)))
          (message "Searching text not found"))
      (message "No search text"))))

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
                (if arg "Backward visit: " "Visit: ")
                (point-min) (point-max)))
         (visit-point (meow--visit-point text reverse)))
    (if visit-point
        (-let* (((marker-beg marker-end) (match-data))
                (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
                (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (-> (meow--make-selection '(select . visit) beg end)
              (meow--select))
          (setq meow--last-search text))
      (message "Searching text not found"))))

(defalias 'meow-occur 'meow-visit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARENS & STRING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--select-common (beg-re end-re type bw)
  (let (mark pos level)
    (save-mark-and-excursion
      (when (re-search-backward beg-re nil t 1)
        (setq mark (point))
        (forward-char 1)
        (setq level (car (syntax-ppss)))
        (while (and (< (point) (point-max))
                    (>= (car (syntax-ppss)) level))
          (forward-char))
        (setq pos (point))))
    (when (and mark pos)
      (-> (meow--make-selection (list 'select type 'outer)
                                (+ mark bw)
                                (- pos bw))
          (meow--select)))))

(defun meow-round-outer ()
  (interactive)
  (meow--select-common "\\((\\)" "\\()\\)" 'round 0))

(defun meow-round-inner ()
  (interactive)
  (meow--select-common "\\((\\)" "\\()\\)" 'round 1))

(defun meow-brace-outer ()
  (interactive)
  (meow--select-common "\\({\\)" "\\(}\\)" 'brace 0))

(defun meow-brace-inner ()
  (interactive)
  (meow--select-common "\\({\\)" "\\(}\\)" 'brace 1))

(defun meow-bracket-outer ()
  (interactive)
  (meow--select-common "\\(\\[\\)" "\\(\\]\\)" 'bracket 0))

(defun meow-bracket-inner ()
  (interactive)
  (meow--select-common "\\(\\[\\)" "\\(\\]\\)" 'bracket 1))

(defun meow--string-common (bw)
  (when (meow--in-string-p)
    (let (mark pos)
      (save-mark-and-excursion
        (while (and (> (point) (point-min))
                    (meow--in-string-p))
          (backward-char 1))
        (setq mark (point)))
      (save-mark-and-excursion
        (while (and (< (point) (point-max))
                    (meow--in-string-p))
          (forward-char 1))
        (setq pos (point)))
      (when (and mark pos)
        (-> (meow--make-selection '(select string outer)
                                  (+ mark bw)
                                  (- pos bw))
            (meow--select))))))

(defun meow-string-outer ()
  (interactive)
  (meow--string-common 0))

(defun meow-string-inner ()
  (interactive)
  (meow--string-common 1))

(defun meow-indent ()
  "Indent region or current line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-indent-region))

(defun meow-M-x ()
  "Just Meta-x."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-excute-extended-command))

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
  (if (not arg)
      (mode-line-other-buffer)
    (split-window)
    (mode-line-other-buffer)))

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

(provide 'meow-command)




;;; meow-command.el ends here
