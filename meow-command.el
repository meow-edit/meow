;;; meow-commands.el --- Commands in Modal On Dvorak
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

;;; Code:

(defun meow--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let ((cmd (key-binding (read-kbd-macro kbd-macro))))
    (call-interactively cmd)))

(defun meow--pop-selection ()
  "Pop a selection from variable `meow--selection-history' and activate."
  (when meow--selection-history
    (let ((sel (pop meow--selection-history)))
      (meow--select-without-history sel))))

(defun meow--make-selection (type mark pos)
  "Make a selection with TYPE, MARK and POINT.

The direction of selection is MARK -> POINT."
  (list type pos mark))

(defun meow--select (selection)
  "Mark the SELECTION."
  (unless (region-active-p)
    (meow--cancel-selection))
  (-let (((sel-type pos mark) selection))
    (if meow--selection
        (push meow--selection meow--selection-history)
      ;; Used to restore the position where we starting selection
      (push (meow--make-selection nil (point) (point))
            meow--selection-history))
    (goto-char pos)
    (when sel-type
      (push-mark mark t t)
      (setq meow--selection selection))))

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
  (if (region-active-p)
      (meow--pop-selection)
    (meow--execute-kbd-macro meow--kbd-undo)))

;;; Words Navigation/Selection

(defun meow-backward-word (arg)
  "Move backward ARG words."
  (interactive "P")
  (-let (((beg . _) (bounds-of-thing-at-point 'word))
         (n (abs (prefix-numeric-value arg)))
         (pos)
         (mark))
    (save-mark-and-excursion
      (when beg (goto-char beg))
      (setq pos (re-search-backward "\\<" nil t n))
      (when pos
        (setq mark (re-search-forward "\\>" nil t 1))))
    (if mark
        (-> (meow--make-selection 'word-mark mark pos)
            (meow--select))
      (message "Backward word failed!"))))

(defun meow-mark-word (arg)
  "From current word, mark ARG words."
  (interactive "P")
  (-let (((beg . _) (bounds-of-thing-at-point 'word))
         (n (abs (prefix-numeric-value arg)))
         (pos)
         (mark))
    (save-mark-and-excursion
      (if beg
          (goto-char beg)
        (re-search-backward "\\<" nil t 1))
      (setq pos (point))
      (setq mark (re-search-forward ".\\>" nil t n)))
    (if mark
        (-> (meow--make-selection 'word-mark mark pos)
            (meow--select))
      (message "Mark word failed!"))))

(defun meow-mark-or-backward-word (arg)
  "Select current word or select previous one if current one is marked already."
  (interactive "P")
  (if (eq 'word-mark (meow--selection-type))
      (meow-backward-word arg)
    (meow-mark-word arg)))

(defun meow-forward-word (arg)
  "Select next word, expanding the region if current selection has type word-mark."
  (interactive "P")
  (cl-case (meow--selection-type)
    ((word-expand word-mark)
     (meow--direction-forward)
     (let* ((n (abs (prefix-numeric-value arg)))
           (mark (mark))
           (pos (re-search-forward ".\\>" nil t n)))
       (if pos
           (-> (meow--make-selection 'word-expand mark pos)
               (meow--select))
         (message "Expand word failed!"))))
    (t
     (-let (((_ . end) (bounds-of-thing-at-point 'word))
            (n (abs (prefix-numeric-value arg)))
            (pos)
            (mark))
       (save-mark-and-excursion
         (when end (goto-char end))
         (setq pos (re-search-forward ".\\>" nil t n))
         (when pos
           (setq mark (re-search-backward "\\<" nil t 1))))
       (if mark
           (-> (meow--make-selection 'word mark pos)
               (meow--select))
         (message "Forward word falied!"))))))

;;; Single Char Navigation/Selection

(defun meow-head (arg)
  "Move towards the head of line."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (when (> (point) (line-beginning-position))
    (backward-char (abs (prefix-numeric-value arg)))))

(defun meow-head-select (arg)
  "Activate selection then move towards the head of line."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (backward-char (abs (prefix-numeric-value arg))))

(defun meow-tail (arg)
  "Move towards the end of line."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (when (< (point) (line-end-position))
    (forward-char (abs (prefix-numeric-value arg)))))

(defun meow-tail-select (arg)
  "Activate selection then move towards the end of line."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (forward-char (abs (prefix-numeric-value arg))))

(defun meow-prev-line (arg)
  "Move previous ARG lines."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (call-interactively #'previous-line))

(defun meow-prev-line-select (arg)
  "Activate selection then move ARG lines up."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (call-interactively #'previous-line))

(defun meow-next-line (arg)
  "Move next ARG lines."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (call-interactively #'next-line))

(defun meow-next-line-select (arg)
  "Activate selection then move ARG lines down."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (call-interactively #'next-line))

;;; Expression Navigation/Selection

(defun meow--scan-sexps (from count)
  "Like function `scan-sexps' with FROM and COUNT.

Wrap with ignore errors."
  (ignore-errors
    (goto-char from)
    (forward-sexp count)
    (point)))

(defun meow-exp (arg)
  "Mark ARG expressions."
  (interactive "P")
  (-let* ((exchange (< (prefix-numeric-value arg) 0))
          (n (abs (prefix-numeric-value arg)))
          (region-beg (when (region-active-p) (region-beginning)))
          (region-end (when (region-active-p) (region-end)))
          (pos)
          (mark)
          (direction-backward (meow--direction-backward-p)))
    (cond
     ((not (eq 'exp (meow--selection-type)))
      (-let (((beg . end) (bounds-of-thing-at-point 'sexp)))
        (save-mark-and-excursion
          (when beg (goto-char beg))
          (save-mark-and-excursion
            (setq pos (meow--scan-sexps (point) n)
                  mark (meow--scan-sexps pos (- n)))))))

     (direction-backward
      (save-mark-and-excursion
        (setq pos (meow--scan-sexps (point) (- n))
              mark (meow--scan-sexps pos 1))))

     (t
      (save-mark-and-excursion
        (setq pos (meow--scan-sexps (point) n)
              mark (meow--scan-sexps pos -1)))))

    (cond
     ((or (not pos) (not mark))
      (if (eq 'exp (meow--selection-type))
          (exchange-point-and-mark)
        (message "Mark exp failed!")))
     (t
      (if exchange
          (-> (meow--make-selection 'exp pos mark)
              (meow--select))
        (-> (meow--make-selection 'exp mark pos)
            (meow--select)))))))

;;; Line Navigation/Selection

(defun meow-line (arg)
  "Select ARG lines."
  (interactive "P")
  (-let* ((exchange (< (prefix-numeric-value arg) 0))
          (n (abs (prefix-numeric-value arg)))
          (direction-backward (meow--direction-backward-p))
          (pos)
          (mark)
          (beg (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end (if (region-active-p) (region-end) (line-end-position))))

    (cond
     ((not (eq 'line (meow--selection-type)))
      (setq beg (line-beginning-position)
            end (save-mark-and-excursion
                  (forward-line (1- n))
                  (line-end-position))))

     (direction-backward
      (setq beg (save-mark-and-excursion
                  (goto-char beg)
                  (forward-line (- n))
                  (line-beginning-position))))

     (t
      (setq end (save-mark-and-excursion
                  (goto-char end)
                  (forward-line (- n (if (= end (line-end-position)) 0 1)))
                  (line-end-position)))))

    (when (= end beg)
      (cl-incf end))

    (if (xor direction-backward exchange)
        (setq pos beg
              mark end)
      (setq mark beg
            pos end))
    (-> (meow--make-selection 'line mark pos)
        (meow--select))))

;;; Block Selection/Expanding

(defun meow--block-indent-fallback ()
  "A fallback behavior on mark block.

Guess block by its indentation."
  (let ((indent (save-mark-and-excursion
                  (meow--direction-backward)
                  (meow--get-indent))))
    (cond
     ((or (zerop indent) (meow--empty-line-p))
      (message "Mark block failed!"))
     (t
      (when (eq 'block (meow--selection-type))
        (goto-char (region-end)))
      (-> (meow--make-selection
           'block-indent
           (save-mark-and-excursion
             (while (and
                     (not (= (line-beginning-position) (point-min)))
                     (or (meow--empty-line-p)
                         (>= (meow--get-indent) indent)))
               (forward-line -1))
             (line-beginning-position))
           (save-mark-and-excursion
             (let ((ret (line-end-position)))
               (while (and
                       (not (= (line-beginning-position) (point-max)))
                       (or (meow--empty-line-p)
                           (>= (meow--get-indent) indent)))
                 (unless (meow--empty-line-p)
                   (setq ret (line-end-position)))
                 (forward-line 1))
               ret)))
          (meow--select))))))

(defun meow-block ()
  "Mark the block or expand to parent block."
  (interactive)
  (if (eq 'block-indent (meow--selection-type))
      (meow--block-indent-fallback)
    (if (meow--in-string-p)
        (let ((end (save-mark-and-excursion
                     (while (and (meow--in-string-p)
                                 (not (= (point) (point-max))))
                       (forward-char))
                     (point)))
              (beg (save-mark-and-excursion
                     (while (and (meow--in-string-p)
                                 (not (= (point) (point-min))))
                       (backward-char))
                     (point))))
          (-> (meow--make-selection 'block beg end)
              (meow--select)))
      (-let (((beg . end) (bounds-of-thing-at-point 'list)))
        (if (and beg (<= beg (point) end))
            (-> (meow--make-selection 'block beg end)
                (meow--select))
          (if (apply #'derived-mode-p meow-indent-block-parser-mode-list)
              (meow--block-indent-fallback)
            (message "Mark block failed!")))))))

;;; exchange mark and point

(defun meow-reverse ()
  "Just exchange point and mark."
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)))

;;; Flip

(defun meow--flip-begin-of-comment ()
  "Mark to the begin of current comment."
  (->> (save-mark-and-excursion
         (1- (re-search-backward "\\s<" nil t 1)))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end-of-comment ()
  "Mark to the end of current comment."
  (->> (save-mark-and-excursion
        (1- (re-search-forward "\\s>" nil t 1)))
      (meow--make-selection 'flip-forward (point))
      (meow--select)))

(defun meow--flip-begin-of-string ()
  "Mark to the begin of current string."
  (->> (save-mark-and-excursion
         (while (and (meow--in-string-p) (> (point) (point-min)))
           (backward-char 1))
         (1+ (point)))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end-of-string ()
  "Mark to the end of current string."
  (->> (save-mark-and-excursion
         (while (and (meow--in-string-p) (< (point) (point-max)))
           (forward-char 1))
         (1- (point)))
       (meow--make-selection 'flip-forward (point))
       (meow--select)))

(defun meow--flip-begin ()
  "Mark to the begin of current block or line."
  (->> (save-mark-and-excursion
         (let ((min (line-beginning-position))
               (ret (point))
               (continue t))
           (while continue
             (unless (meow--scan-sexps ret -1) (setq continue nil))
             (-let (((_ . end) (bounds-of-thing-at-point 'sexp)))
               (if (and end (>= end min))
                   (setq ret (point))
                 (setq continue nil))))
           ret))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end ()
  "Mark to the end of current block or line."
  (->> (save-mark-and-excursion
         (let ((max (line-end-position))
               (ret (point))
               (continue t))
           (while continue
             ;; If no more sexp
             (unless (meow--scan-sexps ret 1) (setq continue nil))
             (-let (((beg . _) (bounds-of-thing-at-point 'sexp)))
               (if (and beg (<= beg max))
                   (setq ret (point))
                 (setq continue nil))))
           ret))
       (meow--make-selection 'flip-forward (point))
       (meow--select)))

(defun meow-flip ()
  "Mark to the end of line(or block) or begin of line(or block)."
  (interactive)
  (let ((sel-type (meow--selection-type)))
    (when (member sel-type '(flip-backward flip-forward))
      (exchange-point-and-mark))
    (cond
     ((meow--in-comment-p)
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin-of-comment)
        (meow--flip-end-of-comment)))
     ((meow--in-string-p)
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin-of-string)
        (meow--flip-end-of-string)))
     (t
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin)
        (meow--flip-end))))))

;;; Forwarding
;; As a replacement for flip.

(defun meow--near (point1 point2 reverse)
  (if reverse
      (max point1 point2)
    (min point1 point2)))

(defun meow--far (point1 point2 reverse)
  (if reverse
      (min point1 point2)
    (max point1 point2)))

(defun meow--toggle-near-far (point1 point2 mark reverse)
  (let ((near (meow--near point1 point2 reverse))
        (far (meow--far point1 point2 reverse)))
    (if (equal (point) near)
        (-> (meow--make-selection 'forwarding mark far)
            (meow--select))
      (-> (meow--make-selection 'forwarding mark near)
          (meow--select)))))

(defun meow--forwarding-end-of-line (mark reverse)
  (save-mark-and-excursion
    (goto-char mark)
    (if reverse
        (line-beginning-position)
      (line-end-position))))

(defun meow--forwarding-comment (mark reverse)
  "Mark to the end of comment or end of line."
  (let ((end-of-comment (save-mark-and-excursion
                          (if reverse
                              (when-let ((pos (re-search-backward "\\s<" nil t 1)))
                                (1+ pos))
                            (when-let ((pos (re-search-forward "\\s>" nil t 1)))
                              (1- pos)))))
        (end-of-line (meow--forwarding-end-of-line mark reverse)))
    (meow--toggle-near-far end-of-line end-of-comment mark reverse)))

(defun meow--forwarding-string (mark reverse)
  "Mark to the end of string or end of line."
  (let ((end-of-string (save-mark-and-excursion
                         (if reverse
                             (progn
                               (while (and (meow--in-string-p) (> (point) (point-min)))
                                 (forward-char -1))
                               (1+ (point)))
                           (progn
                             (while (and (meow--in-string-p) (< (point) (point-max)))
                               (forward-char 1))
                             (1- (point))))))
        (end-of-line (meow--forwarding-end-of-line mark reverse)))
    (meow--toggle-near-far end-of-line end-of-string mark reverse)))

(defun meow--forwarding-default (mark reverse)
  (let ((end-of-block)
        (end-of-line)
        (end (meow--forwarding-end-of-line mark reverse)))
    (save-mark-and-excursion
      (goto-char mark)
      (while (meow--scan-sexps (point) (if reverse -1 1))
        (when (and (if reverse (< (point) end) (> (point) end))
                   (not end-of-line))
          (setq end-of-line (point))))
      (unless end-of-line (setq end-of-line (point)))
      (setq end-of-block (point)))
    (meow--toggle-near-far end-of-line end-of-block mark reverse)))

(defun meow-forwarding (arg)
  "Mark to the end of line(or block)."
  (interactive "P")
  (let ((mark (if (eq 'forwarding (meow--selection-type)) (mark) (point)))
        (reverse (xor (not (= 1 (prefix-numeric-value arg)))
                      (meow--direction-backward-p))))
    (cond
     ((meow--in-comment-p)
      (meow--forwarding-comment mark reverse))

     ((meow--in-string-p)
      (meow--forwarding-string mark reverse))

     (t
      (meow--forwarding-default mark reverse)))))

(defun meow-find-ref ()
  "Xref find."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-find-ref))

(defun meow-pop-marker ()
  "Pop marker."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-pop-marker))

;;; Clipboards

(defun meow-copy ()
  "Copy, like command `kill-ring-save'."
  (interactive)
  (if (region-active-p)
      (meow--execute-kbd-macro meow--kbd-kill-ring-save)
    (message "No selection!")))

(defun meow-yank ()
  "Yank."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-yank))

(defun meow-yank-pop ()
  "Pop yank."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-yank-pop))

;;; Quit

(defun meow-keyboard-quit ()
  "Keyboard quit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (meow--execute-kbd-macro meow--kbd-keyboard-quit)))

(defun meow-quit ()
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
    (delete-window)
    (previous-buffer)))

;;; Comment

(defun meow-comment ()
  "Comment region or comment line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-comment))

;;; Delete Operations

(defun meow-kill ()
  "Kill region or kill line."
  (interactive)
  (if (not (region-active-p))
      (meow--execute-kbd-macro meow--kbd-kill-line)
    (meow--execute-kbd-macro meow--kbd-kill-region)))

(defun meow-join ()
  "Join current line to the previous line.

Known as built-in command `delete-indentation'."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-delete-indentation))

(defun meow-delete ()
  "Forward delete one char."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-delete-char))

(defun meow-zap ()
  "Delete selection, and shrink multiple spaces into one.
If using without selection, toggle the number of spaces between one/zero."
  (interactive)
  (when (region-active-p)
    (meow--execute-kbd-macro meow--kbd-kill-region))
  (let ((cnt 0))
    (save-mark-and-excursion
      (while (equal 32 (char-before))
        (cl-incf cnt)
        (backward-char)))
    (save-mark-and-excursion
      (while (equal 32 (char-after))
        (cl-incf cnt)
        (forward-char)))
    (cond
     ((>= cnt 2) (meow--execute-kbd-macro meow--kbd-just-one-space))
     ((zerop cnt) (meow--execute-kbd-macro meow--kbd-just-one-space))
     ((equal 32 (char-before)) (backward-delete-char 1))
     ((equal 32 (char-after)) (delete-char 1)))))

;;; Toggle Modal State

(defun meow-insert-before ()
  "Move to the begin of selection, switch to INSERT state."
  (interactive)
  (meow--direction-backward)
  (meow--switch-state 'insert))

(defun meow-insert-after ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (meow--direction-forward)
  (meow--switch-state 'insert))

(defun meow-insert-open ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (goto-char (line-end-position))
  (newline-and-indent)
  (meow--switch-state 'insert))

(defun meow-insert-replace ()
  "Kill current selection and switch to INSERT state."
  (interactive)
  (if (not (region-active-p))
      (message "No selection!")
    (meow--execute-kbd-macro meow--kbd-kill-region)
    (meow--switch-state 'insert)))

(defun meow-replace ()
  "Replace current selection with yank."
  (interactive)
  (if (not (region-active-p))
      (message "No selection!")
    (delete-region (region-beginning) (region-end))
    (yank)))

(defun meow-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (when (bound-and-true-p company-mode)
    (when (company--active-p)
      (company-abort)))
  (cond
   (meow-keypad-mode
    (meow-keypad-mode -1))
   ((or multiple-cursors-mode meow-insert-mode)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

;;; Multiple Cursors

(defun meow-select (beg end)
  "Like multiple-cursors' command `mc/mark-all-in-region' from BEGIN to END, but with completion."
  (interactive "r")
  (let* ((search (or search (meow--prompt-symbol-and-words beg end)))
         (case-fold-search nil))
    (if (string= search "")
        (message "Select aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (while (search-forward search end t)
          (push-mark (match-beginning 0))
          (mc/create-fake-cursor-at-point))
        (let ((first (mc/furthest-cursor-before-point)))
          (if (not first)
              (message "Search failed for %S" search)
            (mc/pop-state-from-overlay first)))
        (if (> (mc/num-cursors) 1)
            (multiple-cursors-mode 1)
          (multiple-cursors-mode 0))))))

(defun meow-select-or-skip ()
  "Act as command `meow-select' when multiple cursors is not activated or act like command `mc/skip-to-next-like-this'."
  (interactive)
  (cond
   ((> (mc/num-cursors) 1)
    (call-interactively #'mc/skip-to-next-like-this))
   ((not (region-active-p))
    (message "No selection!"))
   (t
    (call-interactively #'meow-select))))

(defun meow-virtual-cursor ()
  "Just command `mc/mark-next-like-this'."
  (interactive)
  (call-interactively #'mc/mark-next-like-this))

;;; Pagination

(defun meow-page-up ()
  "Page up."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-scoll-down))

(defun meow-page-down ()
  "Page down."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-scoll-up))

;;; Paren Operations

(defun meow-forward-slurp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-forward-slurp))

(defun meow-backward-slurp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-backward-slurp))

(defun meow-forward-barf ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-forward-barf))

(defun meow-backward-barf ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-backward-barf))

(defun meow-raise-sexp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-raise-sexp))

(defun meow-transpose-sexp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-transpose-sexp))

(defun meow-split-sexp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-split-sexp))

(defun meow-join-sexp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-join-sexp))

(defun meow-splice-sexp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-splice-sexp))

(defun meow-wrap-round ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-wrap-round))

(defun meow-wrap-square ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-wrap-square))

(defun meow-wrap-curly ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-wrap-curly))

(defun meow-wrap-string ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-wrap-string))

;;; Others

(defun meow-M-x ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-excute-extended-command))

(defun meow-back-to-indentation ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-back-to-indentation))

(defun meow-indent ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-indent-region))

(defun meow-last-buffer (arg)
  (interactive "P")
  (if (> (mc/num-cursors) 1)
      (message "Multiple Cursors is enabled!")
    (if (not arg)
        (mode-line-other-buffer)
      (split-window)
      (mode-line-other-buffer))))

(defun meow-escape-or-normal-modal ()
  (interactive)
  ;; Cancel company if it is activate.
  (when (bound-and-true-p company-mode)
    (when (company--active-p)
      (company-abort)))
  (cond
   ((minibufferp)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
   (meow-keypad-mode
    (meow-keypad-mode -1))
   ((or multiple-cursors-mode meow-insert-mode)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-space ()
  (interactive)
  (when meow--space-command (call-interactively meow--space-command)))

(defun meow-eval-last-exp ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-eval-last-exp))

(provide 'meow-command)
