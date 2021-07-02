;;; meow-commands.el --- Commands in Meow -*- lexical-binding: t -*-

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
(require 'meow-thing)
(require 'array)

(defun meow--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let ((ret (key-binding (read-kbd-macro kbd-macro))))
    (cond
     ((commandp ret)
      (call-interactively ret))

     ((and (not meow-use-keypad-when-execute-kbd) (keymapp ret))
      (setq which-key-show-transient-maps t)
      (set-transient-map ret nil (lambda()(setq which-key-show-transient-maps nil))))

     ((and meow-use-keypad-when-execute-kbd (keymapp ret))
      (meow--switch-state 'keypad)
      (setq meow--keypad-keys (meow--parse-string-to-keypad-keys kbd-macro))
      (meow--keypad-try-execute)))))

(defmacro meow--with-selection-fallback (&rest body)
  `(if (region-active-p)
       (progn ,@body)
     (meow--selection-fallback)))

(defun meow--selection-fallback ()
  "Run selection fallback commands."
  (if-let ((fallback (alist-get this-command meow-selection-command-fallback)))
      (call-interactively fallback)
    (error "No selection")))

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
      (setq meow--selection selection))))

(defun meow--select-without-history (selection)
  "Mark the SELECTION without recording it in `meow--selection-history'."
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
  "Cancel current selection then undo."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (meow--execute-kbd-macro meow--kbd-undo))

(defun meow-undo-in-selection ()
  "Cancel undo in current region."
  (interactive)
  (when (region-active-p)
    (meow--execute-kbd-macro meow--kbd-undo)))

(defun meow-pop-selection ()
  (interactive)
  (meow--with-selection-fallback
   (meow--pop-selection)
   (when (and (region-active-p) meow--expand-nav-function)
     (meow--maybe-highlight-num-positions))))

(defun meow-pop ()
  "Pop selection or grab."
  (interactive)
  (meow--with-selection-fallback
   (meow-pop-selection)))

(defun meow-pop-all-selection ()
  (interactive)
  (while (meow--pop-selection)))

;;; exchange mark and point

(defun meow-reverse ()
  "Just exchange point and mark.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (meow--with-selection-fallback
   (exchange-point-and-mark)
   (if (member last-command
               '(meow-visit meow-search meow-mark-symbol meow-mark-word))
       (meow--highlight-regexp-in-buffer (car regexp-search-ring))
     (meow--maybe-highlight-num-positions))))

;;; Buffer

(defun meow-begin-of-buffer ()
  "Mark from current point to the beginning of buffer with char selection."
  (interactive)
  (-> (meow--make-selection '(select . transient) (point) (point-min))
    (meow--select)))

(defun meow-end-of-buffer ()
  "Mark from current point to the end of buffer with char selection."
  (interactive)
  (-> (meow--make-selection '(select . transient) (point) (point-max))
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

(defun meow-clipboard-yank ()
  "Yank system clipboard."
  (interactive)
  (call-interactively #'clipboard-yank))

(defun meow-clipboard-kill ()
  "Kill to system clipboard."
  (interactive)
  (call-interactively #'clipboard-kill-region))

(defun meow-clipboard-save ()
  "Save to system clipboard."
  (interactive)
  (call-interactively #'clipboard-kill-ring-save))

(defun meow-save (arg)
  "Copy, like command `kill-ring-save'.

This command supports `meow-selection-command-fallback'.

Use prefix argument to save to secondary selection instead of kill-ring."
  (interactive "P")
  (meow--with-selection-fallback
   (let ((select-enable-clipboard meow-use-clipboard))
     (meow--prepare-region-for-kill)
     (meow--execute-kbd-macro meow--kbd-kill-ring-save))))

(defun meow-save-append ()
  "Copy, like command `kill-ring-save' but append to lastest kill.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (meow--prepare-region-for-kill)
    (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-append (meow--prepare-string-for-kill-append s) nil)
      (deactivate-mark t))))

(defun meow-save-empty ()
  "Copy an empty string, can be used with `meow-save-append' or `meow-kill-append'."
  (interactive)
  (kill-new ""))

(defun meow-save-char ()
  "Copy current char."
  (interactive)
  (when (< (point) (point-max))
    (save-mark-and-excursion
      (goto-char (point))
      (push-mark (1+ (point)) t t)
      (meow--execute-kbd-macro meow--kbd-kill-ring-save))))

(defun meow-yank ()
  "Yank."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (meow--execute-kbd-macro meow--kbd-yank)))

(defun meow-yank-pop ()
  "Pop yank."
  (interactive)
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-yank-pop)))

;;; Quit

(defun meow-cancel-selection ()
  "Cancel selection.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (meow--with-selection-fallback
   (meow--cancel-selection)))

(defun meow-cancel ()
  "Cancel selection or grab.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (meow--with-selection-fallback
   (meow--cancel-selection)))

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

(defun meow-kill (_arg)
  "Kill region.

This command supports `meow-selection-command-fallback'."
  (interactive "P")
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (meow--allow-modify-p)
      (meow--with-selection-fallback
       (cond
        ((equal '(expand . join) (meow--selection-type))
         (delete-indentation nil (region-beginning) (region-end)))
        (t
         (meow--prepare-region-for-kill)
         (meow--execute-kbd-macro meow--kbd-kill-region)))))))

(defun meow-kill-append (_arg)
  "Kill region and append to latest kill.

This command supports `meow-selection-command-fallback'."
  (interactive "P")
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (meow--allow-modify-p)
      (meow--with-selection-fallback
       (cond
        ((equal '(expand . join) (meow--selection-type))
         (delete-indentation nil (region-beginning) (region-end)))
        (t
         (meow--prepare-region-for-kill)
         (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
           (delete-region (region-beginning) (region-end))
           (kill-append (meow--prepare-string-for-kill-append s) nil))))))))

(defun meow-C-k (_arg)
  "Run command on C-k."
  (interactive "P")
  (meow--execute-kbd-macro meow--kbd-kill-line))

(defun meow-kill-whole-line (arg)
  (interactive "P")
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-kill-whole-line)))

(defun meow-backspace ()
  "Backward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (call-interactively #'backward-delete-char)))

(defun meow-delete ()
  "Delete current region.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (when (meow--allow-modify-p)
    (meow--with-selection-fallback
     (cond
      ((equal '(expand . join) (meow--selection-type))
       (delete-indentation nil (region-beginning) (region-end)))
      (t
       (delete-region (region-beginning) (region-end)))))))

(defun meow-C-d ()
  "Run command on C-d."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-delete-char))

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
    (message "Enter temporary normal mode")
    (setq meow--temp-normal t)
    (meow--switch-state 'normal)))

(defun meow-insert ()
  "Move to the start of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-backward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun meow-insert-at-begin ()
  "Move to the start of line, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (goto-char (line-beginning-position))
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun meow-append-at-end ()
  "Move to the end of line, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (goto-char (line-end-position))
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun meow-open-above ()
  "Open a newline above and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'insert)
    (goto-char (line-beginning-position))
    (save-mark-and-excursion
      (newline))
    ;; (save-mark-and-excursion
    ;;   (insert "\n"))
    (indent-for-tab-command)))

(defun meow-open-below ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'insert)
    (goto-char (line-end-position))
    (meow--execute-kbd-macro "RET")))

(defun meow-change ()
  "Kill current selection and switch to INSERT state.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (when (meow--allow-modify-p)
    (meow--with-selection-fallback
     (delete-region (region-beginning) (region-end))
     (meow--switch-state 'insert)
     (setq-local meow--insert-pos (point)))))

(defun meow-change-char ()
  "Delete current char and switch to INSERT state."
  (interactive)
  (when (< (point) (point-max))
    (meow--execute-kbd-macro meow--kbd-delete-char)
    (meow--switch-state 'insert)
    (setq-local meow--insert-pos (point))))

(defun meow-change-save ()
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (and (meow--allow-modify-p) (region-active-p))
      (kill-region (region-beginning) (region-end))
      (meow--switch-state 'insert)
      (setq-local meow--insert-pos (point)))))

(defun meow-replace ()
  "Replace current selection with yank.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (meow--with-selection-fallback
   (let ((select-enable-clipboard meow-use-clipboard))
     (when (meow--allow-modify-p)
       (when-let ((s (string-trim-right (current-kill 0 t) "\n")))
         (delete-region (region-beginning) (region-end))
         (insert s))))))

(defun meow-replace-char ()
  "Replace current char with selection."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (< (point) (point-max))
      (when-let ((s (string-trim-right (current-kill 0 t) "\n")))
        (delete-region (point) (1+ (point)))
        (insert s)))))

(defun meow-replace-save ()
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (meow--allow-modify-p)
      (when-let ((curr (pop kill-ring-yank-pointer)))
        (let ((s (string-trim-right curr "\n")))
          (setq kill-ring kill-ring-yank-pointer)
          (if (region-active-p)
              (let ((old (save-mark-and-excursion
                           (meow--prepare-region-for-kill)
                           (buffer-substring-no-properties (region-beginning) (region-end)))))
                (progn
                  (delete-region (region-beginning) (region-end))
                  (insert s)
                  (kill-new old)))
            (insert s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHAR MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-head (arg)
  "Move towards the head of this line.

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
  "Move towards the end of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to end of line.
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
  "Activate char selection, then move towards the head of this line.

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
  "Activate char selection, then move towards the end of this line.

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

(defun meow-left ()
  "Move to left.

Will cancel all other selection, except char selection. "
  (interactive)
  (when (and (region-active-p)
             (not (equal '(expand . char) (meow--selection-type))))
    (meow-cancel-selection))
  (meow--execute-kbd-macro meow--kbd-backward-char))

(defun meow-right ()
  "Move to right.

Will cancel all other selection, except char selection. "
  (interactive)
  (when (and (region-active-p)
             (not (equal '(expand . char) (meow--selection-type))))
    (meow-cancel-selection))
  (meow--execute-kbd-macro meow--kbd-forward-char))

(defun meow-left-expand ()
  "Activate char selection, then move left."
  (interactive)
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
      (meow--select)))
  (meow--execute-kbd-macro meow--kbd-backward-char))

(defun meow-right-expand ()
  "Activate char selection, then move right."
  (interactive)
  (if (region-active-p)
      (-> (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (-> (meow--make-selection '(expand . char) (point) (point))
      (meow--select)))
  (meow--execute-kbd-macro meow--kbd-forward-char))

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
    (setq this-command #'previous-line)
    (meow--execute-kbd-macro meow--kbd-backward-line))))

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
    (setq this-command #'next-line)
    (meow--execute-kbd-macro meow--kbd-forward-line))))

(defun meow-prev-expand (arg)
  "Activate char selection, then move to the previous line.

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
    (setq this-command #'previous-line)
    (meow--execute-kbd-macro meow--kbd-backward-line))))

(defun meow-next-expand (arg)
  "Activate char selection, then move to the next line.

See `meow-next-line' for how prefix arguments work."
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
    (setq this-command #'next-line)
    (meow--execute-kbd-macro meow--kbd-forward-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORD/SYMBOL MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-mark-word (n)
  (interactive "p")
  (-let* (((beg . end) (bounds-of-thing-at-point 'word)))
    (when beg
      (-> (meow--make-selection '(expand . word) beg end)
        (meow--select (< n 0)))
      (let ((search (format "\\<%s\\>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (meow--push-search search)
        (meow--highlight-regexp-in-buffer search)))))

(defun meow-mark-symbol (n)
  (interactive "p")
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol)))
    (when beg
      (-> (meow--make-selection '(expand . word) beg end)
        (meow--select (< n 0)))
      (let ((search (format "\\_<%s\\_>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (meow--push-search search)
        (meow--highlight-regexp-in-buffer search)))))

(defun meow--forward-symbol-1 ()
  (forward-symbol 1))

(defun meow--backward-symbol-1 ()
  (let ((pos (point)))
    (forward-symbol -1)
    (not (= pos (point)))))

(defun meow-next-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (_ (when expand (meow--direction-forward)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (forward-word n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(backward-word . forward-word)))))

(defun meow-next-symbol (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (_ (when expand (meow--direction-forward)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (forward-symbol n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

(defun meow-back-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (_ (when expand (meow--direction-backward)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (when (backward-word n)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(backward-word . forward-word)))))

(defun meow-back-symbol (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (meow--direction-backward)
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
         (_ (when expand (meow--direction-backward)))
         (type (if expand '(expand . word) '(select . word)))
         (m (point))
         (p (save-mark-and-excursion
              (forward-symbol (- n))
              (unless (= (point) m)
                (point)))))
    (when p
      (-> (meow--make-selection type m p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINE SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--forward-line-1 ()
  (forward-line 1)
  (if meow--expanding-p
      (goto-char (line-end-position))
    (goto-char (line-beginning-position))))

(defun meow--backward-line-1 ()
  (forward-line -1)
  (goto-char (line-beginning-position)))

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
        (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1))))
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
                     (when (meow--empty-line-p)
                       (backward-char 1))
                     (line-beginning-position))))))
        (-> (meow--make-selection '(expand . line) m p expand)
          (meow--select))
        (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1)))))))

(defun meow-line-expand (n)
  "Like `meow-line', but always expand."
  (interactive "p")
  (meow-line n t))

(defun meow-goto-line (arg)
  "Goto line, recenter and select that line."
  (interactive "P")
  (let (beg
        end
        (ln (when arg (prefix-numeric-value arg))))
    (save-mark-and-excursion
      (while (not ln)
        (let* ((input (read-from-minibuffer "Goto line: ")))
          (if (string-match-p "[[:digit:]]+" input)
              (setq ln (string-to-number input))
            (message "please enter a number."))))
      (goto-char (point-min))
      (forward-line (1- ln))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (-> (meow--make-selection '(expand . line) beg end nil)
      (meow--select))
    (recenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun meow--backward-block ()
  (let ((pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-backward "\\s(" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when pos (goto-char pos))))

(defun meow--forward-block ()
  (let ((pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-forward "\\s)" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when pos (goto-char pos))))

(defun meow-block (arg)
  "Mark the block or expand to parent block."
  (interactive "P")
  (unless (equal 'block (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let ((ra (region-active-p))
        (back (xor (meow--direction-backward-p) (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                  (or (meow--in-string-p)
                      (if ra (>= (car (syntax-ppss)) depth) (> (car (syntax-ppss)) depth)))))
      (when (and (if ra (< (car (syntax-ppss)) depth) (<= (car (syntax-ppss)) depth))
                 (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)))
          (setq m (point)))))
    (when (and p m)
      (-> (meow--make-selection '(expand . block) m p)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-block . meow--forward-block)))))

(defun meow-block-expand (arg)
  "Expand to next block.

Will create selection with type (expand . block)."
  (interactive "P")
  ;; We respect the direction of block selection.
  (let ((back (or (when (equal 'block (cdr (meow--selection-type)))
                     (meow--direction-backward-p))
                  (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                  (or (meow--in-string-p)
                      (> (car (syntax-ppss)) depth))))
      (when (and (= (car (syntax-ppss)) depth)
                 (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)))
          (setq m (point)))))
    (when (and p m)
      (-> (meow--make-selection '(expand . block) orig-pos p t)
          (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-block . meow--forward-block)))))

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
      (-> (meow--make-selection '(expand . join) pos mark)
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
    (-> (meow--make-selection '(expand . join) mark pos)
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
    (-> (meow--make-selection '(expand . join) mark pos)
      (meow--select))))

(defun meow-join (arg)
  "Select the indentation between this line to the non empty previous line.

Will create selection with type (select . join)

Prefix:
with NEGATIVE ARGUMENT, forward search indentation to select.
with UNIVERSAL ARGUMENT, search both side."
  (interactive "P")
  (cond
   ((or (equal '(expand . join) (meow--selection-type))
        (meow--with-universal-argument-p arg))
    (meow--join-both))
   ((meow--with-negative-argument-p arg)
    (meow--join-forward))
   (t
    (meow--join-backward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND & TILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--find-continue-forward ()
  (when meow--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-find)))
      (search-forward ch-str nil t 1))))

(defun meow--find-continue-backward ()
  (when meow--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-find)))
      (search-backward ch-str nil t 1))))

(defun meow--till-continue-forward ()
  (when meow--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-till)))
      (when (< (point) (point-max))
        (forward-char 1)
        (when (search-forward ch-str nil t 1)
          (backward-char 1)
          t)))))

(defun meow--till-continue-backward ()
  (when meow--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-till)))
      (when (> (point) (point-min))
        (backward-char 1)
        (when (search-backward ch-str nil t 1)
          (forward-char 1)
          t)))))

(defun meow-find (n &optional prompt expand)
  "Find the next N char read from minibuffer."
  (interactive "p")
  (let* ((case-fold-search nil)
         (ch (read-char (or prompt (message "Find(%d):" n))))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end)
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (-> (meow--make-selection '(select . find)
                                beg end expand)
        (meow--select))
      (setq meow--last-find ch)
      (meow--maybe-highlight-num-positions
       '(meow--find-continue-backward . meow--find-continue-forward)))))

(defun meow-find-expand (n)
  (interactive "p")
  (meow-find n (message "Expand find(%d):" n) t))

(defun meow-till (n &optional prompt expand)
  "Forward till the next N char read from minibuffer."
  (interactive "p")
  (let* ((case-fold-search nil)
         (ch (read-char (message (or prompt "Till(%d):") n)))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (-> (meow--make-selection '(select . till)
                                beg (+ end fix-pos) expand)
        (meow--select))
      (setq meow--last-till ch)
      (meow--maybe-highlight-num-positions
       '(meow--till-continue-backward . meow--till-continue-forward)))))

(defun meow-till-expand (n)
  (interactive "p")
  (meow-till n (message "Expand till(%d):" n) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISIT and SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-search (arg)
  "Searching for the same text in selection or next visited text."
  (interactive "P")
  ;; Test if we add current region as search target.
  (when (and (region-active-p)
             (let ((search (car regexp-search-ring)))
               (or (not search)
                   (not (string-match-p
                         (format "^%s$" search)
                         (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (meow--push-search (buffer-substring-no-properties (region-beginning) (region-end))))
  (when-let ((search (car regexp-search-ring)))
    (let ((reverse (xor (meow--with-negative-argument-p arg) (meow--direction-backward-p)))
          (case-fold-search nil))
      (if (or (if reverse
                  (re-search-backward search nil t 1)
                (re-search-forward search nil t 1))
              ;; Try research from buffer beginning/end
              ;; if we are already at the last/first matched
              (save-mark-and-excursion
                ;; Recalculate search indicator
                (meow--clean-search-indicator-state)
                (goto-char (if reverse (point-max) (point-min)))
                (if reverse
                    (re-search-backward search nil t 1)
                  (re-search-forward search nil t 1))))
          (-let* (((marker-beg marker-end) (match-data))
                  (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                  (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
            (-> (meow--make-selection '(select . visit) beg end)
              (meow--select))
            (if reverse
                (message "Reverse search: %s" search)
              (message "Search: %s" search))
            (meow--ensure-visible))
        (message "Searching %s failed" search))
      (meow--highlight-regexp-in-buffer search))))

(defun meow-pop-search ()
  "Searching for the previous target."
  (interactive)
  (when-let ((search (pop regexp-search-ring)))
    (message "current search is: %s" (car regexp-search-ring))
    (meow--cancel-selection)))

(defun meow--visit-point (text reverse)
  "Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
  (let ((func (if reverse #'re-search-backward #'re-search-forward))
        (func-2 (if reverse #'re-search-forward #'re-search-backward))
        (case-fold-search nil))
    (save-mark-and-excursion
      (or (funcall func text nil t 1)
          (funcall func-2 text nil t 1)))))

(defun meow-visit (arg)
  "Mark the search text.
Argument ARG if not nil, reverse the selection when making selection."
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
          (meow--push-search text)
          (meow--ensure-visible)
          (meow--highlight-regexp-in-buffer text)
          (setq meow--dont-remove-overlay t))
      (message "Visit: %s failed" text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-thing-prompt (prompt-text)
  (read-char
   (if meow-display-thing-help
       (concat (meow--render-char-thing-table) "\n" prompt-text)
     prompt-text)))

(defun meow-beginning-of-thing ()
  "Select to the beginning of thing represented by CH.
When EXPAND is non-nil, extend current selection.

Prefix argument is not allowed for this command."
  (interactive)
  (save-window-excursion
    (let ((bounds (meow--parse-inner-of-thing-char
                   (meow-thing-prompt "Beginning of:"))))
      (when bounds
        (-> (meow--make-selection '(select . transient)
                                  (point)
                                  (car bounds))
          (meow--select))))))

(defun meow-end-of-thing ()
  "Select to the end of thing represented by CH.
When EXPAND is non-nil, extend current selection.

Prefix argument is not allowed for this command."
  (interactive)
  (save-window-excursion
    (let ((bounds (meow--parse-inner-of-thing-char
                   (meow-thing-prompt "End of:"))))
      (when bounds
        (-> (meow--make-selection '(select . transient)
                                  (point)
                                  (cdr bounds))
          (meow--select))))))

(defun meow-inner-of-thing ()
  (interactive)
  (save-window-excursion
    (let ((bounds (meow--parse-inner-of-thing-char
                   (meow-thing-prompt "Inner of:"))))
      (when bounds
        (-> (meow--make-selection '(select . transient)
                                  (car bounds)
                                  (cdr bounds))
          (meow--select))))))

(defun meow-bounds-of-thing ()
  (interactive)
  (save-window-excursion
    (let ((bounds (meow--parse-bounds-of-thing-char
                   (meow-thing-prompt "Bounds of:"))))
      (when bounds
        (-> (meow--make-selection '(select . transient)
                                  (car bounds)
                                  (cdr bounds))
          (meow--select))))))

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

(defun meow-query-replace ()
  "Query replace."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-query-replace))

(defun meow-query-replace-regexp ()
  "Query replace regexp."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-query-replace-regexp))

(defun meow-last-buffer (arg)
  "Switch to last buffer.
Argument ARG if not nil, switching in a new window."
  (interactive "P")
  (cond
   ((minibufferp)
    (keyboard-escape-quit))
   ((not arg)
    (mode-line-other-buffer))
   (t)))

(defun meow-minibuffer-quit ()
  "Keyboard escape quit in minibuffer."
  (interactive)
  (if (fboundp 'minibuffer-keyboard-quit)
      (call-interactively #'minibuffer-keyboard-quit)
    (call-interactively #'abort-recursive-edit)))

(defun meow-escape-or-normal-modal ()
  "Keyboard escape quit or switch to normal state."
  (interactive)
  (cond
   ((minibufferp)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
   ;; ((meow-keypad-mode-p)
   ;;  (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))
   ((eq major-mode 'fundamental-mode)
    (meow--switch-state 'normal))))

(defun meow-motion-origin-command ()
  "Execute the original command bound in special mode."
  (interactive)
  (let ((key (string last-input-event)))
    (when-let (cmd (meow--get-origin-command key))
      (call-interactively cmd))))

(defun meow-eval-last-exp ()
  "Eval last sexp."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-eval-last-exp))

(defun meow-expand (&optional n)
  (interactive)
  (when (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
    (let* ((n (or n (string-to-number (char-to-string last-input-event))))
           (n (if (= n 0) 10 n))
           (sel-type (cons 'expand (cdr (meow--selection-type)))))
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
      (meow--maybe-highlight-num-positions meow--expand-nav-function))))

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

(defun meow-digit-argument ()
  (interactive)
  (set-transient-map meow-numeric-argument-keymap)
  (call-interactively #'digit-argument))

(defun meow-universal-argument ()
  "Replacement for universal-argument."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'universal-argument-more)
    (call-interactively 'universal-argument)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KMACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-kmacro-lines ()
  "Apply KMacro to each line in region."
  (interactive)
  (meow--with-selection-fallback
   (-let (((beg . end) (car (region-bounds))))
     (meow--wrap-collapse-undo
       (apply-macro-to-region-lines beg end)))))

(defun meow-kmacro-matches (arg)
  "Apply KMacro by search.

Use negative argument for backward application."
  (interactive "P")
  (let ((s (car regexp-search-ring))
        (case-fold-search nil)
        (back (meow--with-negative-argument-p arg)))
    (meow--wrap-collapse-undo
      (while (if back
                 (re-search-backward s nil t)
               (re-search-forward s nil t))
        (-> (meow--make-selection '(select . visit)
                                  (if back
                                      (point)
                                    (match-beginning 0))
                                  (if back
                                      (match-end 0)
                                    (point)))
          (meow--select))
        (let ((ov (make-overlay (region-beginning) (region-end))))
          (unwind-protect
              (progn
                (kmacro-call-macro nil))
            (let ((pos (point)))
              (if back
                  (goto-char (min (point) (overlay-start ov)))
                (goto-char (max (point) (overlay-end ov))))
              (delete-overlay ov))))))))

(defun meow-start-kmacro ()
  "Start kmacro.

This command is a replacement for build-in `kmacro-start-macro'."
  (interactive)
  (if (or (meow-normal-mode-p) (meow-motion-mode-p))
      (call-interactively #'kmacro-start-macro)
    (message "Can only start recording in NORMAL or MOTION state.")))

(defun meow-start-kmacro-or-insert-counter ()
  "Start kmacro or insert counter.

This command is a replacement for build-in `kmacro-start-macro-or-insert-counter'."
  (interactive)
  (cond
   ((or defining-kbd-macro executing-kbd-macro)
     (call-interactively #'kmacro-insert-counter))
   ((or (meow-normal-mode-p) (meow-motion-mode-p))
    (call-interactively #'kmacro-start-macro-or-insert-counter))
   (t (message "Can only start recording in NORMAL or MOTION state."))))

(defun meow-end-or-call-kmacro ()
  "End kmacro recording or call macro.

This command is a replacement for build-in `kmacro-end-or-call-macro'."
  (interactive)
  (cond
   ((and (equal this-original-command 'meow-keypad-self-insert)
         defining-kbd-macro)
    (message "Can't end kmacro with KEYPAD command"))
   ((or (meow-normal-mode-p)
        (meow-motion-mode-p))
    (call-interactively #'kmacro-end-or-call-macro))
   (t
    (message "Can only end or call kmacro in NORMAL or MOTION state."))))

(defun meow-end-kmacro ()
  "End kmacro recording or call macro.

This command is a replacement for build-in `kmacro-end-macro'."
  (interactive)
  (cond
   ((equal this-original-command 'meow-keypad-self-insert)
    (message "Can't end kmacro with KEYPAD command"))
   ((or (meow-normal-mode-p)
        (meow-motion-mode-p))
    (call-interactively #'kmacro-end-or-call-macro))
   (t
    (message "Can only end or call kmacro in NORMAL or MOTION state."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAB SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (progn
        (setq meow--secondary-selection meow--selection)
        (secondary-selection-from-region))
    (delete-overlay mouse-secondary-overlay)
	(setq mouse-secondary-start (make-marker))
    (move-marker mouse-secondary-start (point)))
  (meow--cancel-selection))

(defun meow-pop-grab ()
  "Pop to secondary selection."
  (interactive)
  (cond
   ((meow--second-sel-buffer)
    (pop-to-buffer (meow--second-sel-buffer))
    (secondary-selection-to-region)
    (setq mouse-secondary-start (make-marker))
    (move-marker mouse-secondary-start (point)))
   ((markerp mouse-secondary-start)
    (or
     (when-let ((buf (marker-buffer mouse-secondary-start)))
       (pop-to-buffer buf)
       (when-let ((pos (marker-position mouse-secondary-start)))
         (goto-char pos)))
     (message "No secondary selection")))))

(defun meow-swap-grab ()
  "Swap region and secondary selection."
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
         (sel-str (meow--second-sel-get-string)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (meow--second-sel-set-string (or region-str ""))))

(defun meow-sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (meow--with-selection-fallback
   (let* ((rbeg (region-beginning))
          (rend (region-end))
          (region-str (buffer-substring-no-properties rbeg rend)))
     (meow--second-sel-set-string region-str))))

;; aliases
(defalias 'meow-backward-delete 'meow-backspace)
(defalias 'meow-c-d 'meow-C-d)
(defalias 'meow-c-k 'meow-C-k)

(provide 'meow-command)
;;; meow-command.el ends here
