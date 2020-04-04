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

(require 'dash)
(require 'multiple-cursors)

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
    (meow--execute-kbd-macro meow--kbd-macro-undo)))

;;; Words Navigation/Selection
;; Meow has two commands: m/w for word navigation/selection.
;; Use w to mark next word.
;; Use m to mark current word, if it is already marked, press again will mark previous word.
;; The selection activated by m has a type word-mark.
;; When current selection is a word-mark selection, w will expand selection by word, and change selection type to word-expand.
;; When selection has type word-expand, m will cancel the selection and mark only current word.

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
;; Defaults: h/t/n/p use with Shift to activate selection.
;; Once the selection is activate, you don't have to hold the shift key for following single char movements.

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
  (previous-line (abs (prefix-numeric-value arg))))

(defun meow-prev-line-select (arg)
  "Activate selection then move previous ARG lines."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (previous-line (abs (prefix-numeric-value arg))))

(defun meow-next-line (arg)
  "Move next ARG lines."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (forward-line (abs (prefix-numeric-value arg))))

(defun meow-next-line-select (arg)
  "Activate selection then move next ARG lines."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (forward-line (abs (prefix-numeric-value arg))))

;;; Expression Navigation/Selection

(defun meow--scan-sexps (from count)
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
      (setq end (save-mark-and-excursion
                  (forward-line (- n 1))
                  (line-end-position))))

     (direction-backward
      (setq beg (save-mark-and-excursion
                  (goto-char beg)
                  (previous-line n)
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

(defun meow-block ()
  "Mark the block or expand to parent block."
  (interactive)
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
      (if beg
          (-> (meow--make-selection 'block beg end)
              (meow--select))
        (message "Mark block failed!")))))

;;; Flip mark and point

(defun meow-flip ()
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)))

;;; Extend
;;
;; h - To the beginning of line
;; t - To the end of line
;; e - To the end of the last expression in current line.
;; b - To the end of current block.
;; w - To the end of current symbol.
;; x - To the beginninng of buffer.
;; l - Rest lines in buffer.
;; p - Whole paragraph.
;; n - Inside nested expression.

(defun meow-region ()
  (interactive)
  (-let* ((ch (read-char "Region:"))    ; multiple cursors aware
          ((mark . pos)
           (cl-case ch
             ;; Beginning of current line
             (?h (cons (point) (line-beginning-position)))
             ;; End of current line
             (?t (cons (point) (line-end-position)))
             ;; Last expression in current line
             (?e (cons (point)
                       (save-mark-and-excursion
                         (let ((ret (point))
                               (end (line-end-position))
                               (continue t))
                           (while continue
                             (setq continue
                                   (and (meow--scan-sexps ret 1)
                                        (<= (point) end)))
                             (when continue (setq ret (point))))
                           ret))))
             ;; End of block
             (?b (cons (point)
                       (if (meow--in-comment-p)
                           (save-mark-and-excursion
                             (while (and (meow--in-comment-p)
                                         (not (= (point) (point-max))))
                               (forward-char))
                             (1- (point)))
                         (if (meow--in-string-p)
                             (save-mark-and-excursion
                               (while (and (meow--in-string-p)
                                           (not (= (point) (point-max))))
                                 (forward-char))
                               (1- (point)))
                           (save-mark-and-excursion
                             (let ((ret))
                               (while (meow--scan-sexps (point) 1)
                                 (setq ret (point)))
                               ret))))))
             ;; To the end of word.
             (?m (cons (point) (save-mark-and-excursion (re-search-forward "\\>" nil t 1))))
             ;; To the end of symbol.
             (?w (cons (point) (save-mark-and-excursion (re-search-forward "\\_>" nil t 1))))
             ;; To the end of buffer
             (?l (cons (line-end-position) (point-max)))
             ;; To the beginning of buffer
             (?r (cons (point) (point-min)))
             ;; Whole paragrah
             (?p (bounds-of-thing-at-point 'paragraph))
             ;; Inside nested block
             (?n (save-mark-and-excursion
                   (when (re-search-forward "\\s(" nil t 1)
                     (cons (save-mark-and-excursion
                             (let ((ret))
                               (while (meow--scan-sexps (point) 1)
                                 (setq ret (point)))
                               ret))
                           (point))))))))
    (if (and pos (not (= pos (point))))
        (-> (meow--make-selection 'extend mark pos)
            (meow--select))
      (message "Mark failed!"))))

;;; XRef

(defun meow-find-ref ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-macro-find-ref))

(defun meow-pop-marker ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-macro-pop-marker))

;;; Clipboards

(defun meow-copy ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-kill-ring-save))

(defun meow-yank ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-yank))

(defun meow-yank-pop ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-yank-pop))

;;; Quit

(defun meow-keyboard-quit ()
  "Keyboard quit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (meow--execute-kbd-macro meow--kbd-macro-keyboard-quit)))

(defun meow-quit ()
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
    (delete-window)
    (previous-buffer)))

;;; Comment

(defun meow-comment ()
  "Comment region or comment line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-macro-comment))

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
  (interactive)
  (meow--direction-backward)
  (meow--cancel-selection)
  (meow--switch-state 'insert))

(defun meow-insert-after ()
  (interactive)
  (meow--direction-forward)
  (meow--cancel-selection)
  (meow--switch-state 'insert))

(defun meow-insert-open ()
  (interactive)
  (meow--cancel-selection)
  (goto-char (line-end-position))
  (newline-and-indent)
  (meow--switch-state 'insert))

(defun meow-insert-replace ()
  (interactive)
  (if (not (region-active-p))
      (message "No selection!")
    (meow--execute-kbd-macro meow--kbd-kill-region)
    (meow--switch-state 'insert)))

(defun meow-insert-exit ()
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

(defun meow-select (beg end &optional search)
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
  (interactive)
  (cond
   ((> (mc/num-cursors) 1)
    (call-interactively #'mc/skip-to-next-like-this))
   ((not (region-active-p))
    (message "No selection!"))
   (t
    (call-interactively #'meow-select))))

(defun meow-virtual-cursor ()
  (interactive)
  (call-interactively #'mc/mark-next-like-this))

;;; Pagination

(defun meow-page-up ()
  (interactive)
  (meow--execute-kbd-macro meow--kbd-scoll-down))

(defun meow-page-down ()
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

(defun meow-raise-exp ()
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
    (call-interactively #'minibuffer-keyboard-quit))
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
