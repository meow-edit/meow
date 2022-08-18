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
(require 'subr-x)
(require 'seq)

(require 'meow-var)
(require 'meow-util)
(require 'meow-visual)
(require 'meow-thing)
(require 'meow-beacon)
(require 'meow-keypad)
(require 'array)

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
  (let ((sel-type (car selection))
        (mark (cadr selection))
        (pos (caddr selection)))
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
  (let ((sel-type (car selection))
        (mark (cadr selection))
        (pos (caddr selection)))
    (goto-char pos)
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

(defun meow-save ()
  "Copy, like command `kill-ring-save'.

This command supports `meow-selection-command-fallback'."
  (interactive)
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
  "Kill region.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (meow--allow-modify-p)
      (meow--with-selection-fallback
       (cond
        ((equal '(expand . join) (meow--selection-type))
         (delete-indentation nil (region-beginning) (region-end)))
        (t
         (meow--prepare-region-for-kill)
         (meow--execute-kbd-macro meow--kbd-kill-region)))))))

(defun meow-kill-append ()
  "Kill region and append to latest kill.

This command supports `meow-selection-command-fallback'."
  (interactive)
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

(defun meow-C-k ()
  "Run command on C-k."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-kill-line))

(defun meow-kill-whole-line ()
  (interactive)
  (when (meow--allow-modify-p)
    (meow--execute-kbd-macro meow--kbd-kill-whole-line)))

(defun meow-backspace ()
  "Backward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (call-interactively #'backward-delete-char)))

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
   ((and (meow-insert-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-insert-exit))
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
    (meow--cancel-selection)
    (meow--switch-state 'insert)))

(defun meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and meow-use-cursor-position-hack
                   (< (point) (point-max)))
          (forward-char 1))
      (meow--direction-forward)
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
    (indent-according-to-mode)))

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
    (setq this-command #'meow-change)
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
  (let ((ra (region-active-p)))
    (when (and ra
           (not (equal '(expand . char) (meow--selection-type))))
      (meow-cancel-selection))
    (when (or (not meow-use-cursor-position-hack)
              (not ra)
              (equal '(expand . char) (meow--selection-type)))
      (meow--execute-kbd-macro meow--kbd-forward-char))))

(defun meow-left-expand ()
  "Activate char selection, then move left."
  (interactive)
  (if (region-active-p)
      (thread-first
        (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (when meow-use-cursor-position-hack
      (forward-char 1))
    (thread-first
      (meow--make-selection '(expand . char) (point) (point))
      (meow--select)))
  (meow--execute-kbd-macro meow--kbd-backward-char))

(defun meow-right-expand ()
  "Activate char selection, then move right."
  (interactive)
  (if (region-active-p)
      (thread-first
        (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (thread-first
      (meow--make-selection '(expand . char) (point) (point))
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
      (thread-first
        (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (thread-first
      (meow--make-selection '(expand . char) (point) (point))
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
      (thread-first
        (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (thread-first
      (meow--make-selection '(expand . char) (point) (point))
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
  "Mark current word under cursor.

A expandable word selection will be created. `meow-next-word' and
`meow-back-word' can be used for expanding.

The content of selection will be quoted to regexp, then pushed into
`regexp-search-ring' which be read by `meow-search' and other commands.

This command will also provide highlighting for same occurs.

Use negative argument to create a backward selection."
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (meow--make-selection '(expand . word) beg end)
        (meow--select (< n 0)))
      (let ((search (format "\\<%s\\>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (meow--push-search search)
        (meow--highlight-regexp-in-buffer search)))))

(defun meow-mark-symbol (n)
  "Mark current symbol under cursor.

This command works similar to `meow-mark-word'."
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (meow--make-selection '(expand . word) beg end)
        (meow--select (< n 0)))
      (let ((search (format "\\_<%s\\_>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (meow--push-search search)
        (meow--highlight-regexp-in-buffer search)))))

(defun meow--forward-symbol-1 ()
  (when (forward-symbol 1)
    (meow--hack-cursor-pos (point))))

(defun meow--backward-symbol-1 ()
  (let ((pos (point)))
    (forward-symbol -1)
    (when (not (= pos (point)))
      (point))))

(defun meow--fix-word-selection-mark (pos mark)
  "Return new mark for a word select.
This will shrink the word selection only contains
 word/symbol constituent character and whitespaces."
  (save-mark-and-excursion
    (goto-char pos)
    (if (> mark pos)
        (progn (skip-syntax-forward "-_w" mark)
               (point))
      (skip-syntax-backward "-_w" mark)
      (point))))

(defun meow--forward-word-1 ()
  (when (forward-word)
    (meow--hack-cursor-pos (point))))

(defun meow--backward-word-1 ()
  (when (forward-word -1)
    (point)))

(defun meow-next-word (n)
  "Select to the end of the next Nth word.

A non-expandable, word selection will be created.

To select continuous words, use following approaches:

1. start the selection with `meow-mark-word'.

2. use prefix digit arguments.

3. use `meow-expand' after this command.
"
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
      (thread-first
        (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-word-1 . meow--forward-word-1)))))

(defun meow-next-symbol (n)
  "Select to the end of the next Nth symbol.

A non-expandable, word selection will be created.
There's no symbol selection type in Meow.

To select continuous symbols, use following approaches:

1. start the selection with `meow-mark-symbol'.

2. use prefix digit arguments.

3. use `meow-expand' after this command."
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
      (thread-first
        (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

(defun meow-back-word (n)
  "Select to the beginning the previous Nth word.

A non-expandable word selection will be created.
This command works similar to `meow-next-word'."
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
      (thread-first
        (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-word-1 . meow--forward-word-1)))))

(defun meow-back-symbol (n)
  "Select to the beginning the previous Nth symbol.

A non-expandable word selection will be created.
This command works similar to `meow-next-symbol'."
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
      (thread-first
        (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-symbol-1 . meow--forward-symbol-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINE SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--forward-line-1 ()
  (let ((orig (point)))
    (forward-line 1)
    (if meow--expanding-p
        (progn
          (goto-char (line-end-position))
          (line-end-position))
      (when (< orig (line-beginning-position))
        (line-beginning-position)))))

(defun meow--backward-line-1 ()
  (forward-line -1)
  (line-beginning-position))

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
  (let* ((orig (mark t))
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
        (thread-first
          (meow--make-selection '(expand . line) orig p expand)
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
        (thread-first
          (meow--make-selection '(expand . line) m p expand)
          (meow--select))
        (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1)))))))

(defun meow-line-expand (n)
  "Like `meow-line', but always expand."
  (interactive "p")
  (meow-line n t))

(defun meow-goto-line ()
  "Goto line, recenter and select that line.

This command will expand line selection."
  (interactive)
  (let* ((rbeg (when (use-region-p) (region-beginning)))
         (rend (when (use-region-p) (region-end)))
         (expand (equal '(expand . line) (meow--selection-type)))
         (orig-p (point))
         (beg-end (save-mark-and-excursion
                    (if meow-goto-line-function
                      (call-interactively meow-goto-line-function)
                      (meow--execute-kbd-macro meow--kbd-goto-line))
                    (cons (line-beginning-position)
                          (line-end-position))))
         (beg (car beg-end))
         (end (cdr beg-end)))
    (thread-first
      (meow--make-selection '(expand . line)
                            (if (and expand rbeg) (min rbeg beg) beg)
                            (if (and expand rend) (max rend end) end))
      (meow--select (> orig-p beg)))
    (recenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--backward-block ()
  (let ((orig-pos (point))
        (pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-backward "\\s(" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when (and pos (not (= orig-pos pos)))
      (goto-char pos))))

(defun meow--forward-block ()
  (let ((orig-pos (point))
        (pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-forward "\\s)" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when (and pos (not (= orig-pos pos)))
      (goto-char pos)
      (meow--hack-cursor-pos (point)))))

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
      (thread-first
        (meow--make-selection '(expand . block) m p)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-block . meow--forward-block)))))

(defun meow-to-block (arg)
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
      (thread-first
        (meow--make-selection '(expand . block) orig-pos p t)
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
      (thread-first
        (meow--make-selection '(expand . join) pos mark)
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
    (thread-first
      (meow--make-selection '(expand . join) mark pos)
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
    (thread-first
      (meow--make-selection '(expand . join) mark pos)
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
      (when (search-forward ch-str nil t 1)
        (meow--hack-cursor-pos (point))))))

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
          (meow--hack-cursor-pos (point)))))))

(defun meow--till-continue-backward ()
  (when meow--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-till)))
      (when (> (point) (point-min))
        (backward-char 1)
        (when (search-backward ch-str nil t 1)
          (forward-char 1)
          (point))))))

(defun meow-find (n ch &optional expand)
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
      (thread-first
        (meow--make-selection '(select . find)
                              beg end expand)
        (meow--select))
      (setq meow--last-find ch)
      (meow--maybe-highlight-num-positions
       '(meow--find-continue-backward . meow--find-continue-forward)))))

(defun meow-find-expand (n ch)
  (interactive "p\ncExpand find:")
  (meow-find n ch t))

(defun meow-till (n ch &optional expand)
  "Forward till the next N char read from minibuffer."
  (interactive "p\ncTill:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (thread-first
        (meow--make-selection '(select . till)
                              beg (+ end fix-pos) expand)
        (meow--select))
      (setq meow--last-till ch)
      (meow--maybe-highlight-num-positions
       '(meow--till-continue-backward . meow--till-continue-forward)))))

(defun meow-till-expand (n ch)
  (interactive "p\ncExpand till:")
  (meow-till n ch t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISIT and SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow-search (arg)
  " Search and select with the car of current `regexp-search-ring'.

If the contents of selection doesn't match the regexp, will push it to `regexp-search-ring' before searching.

To search backward, use \\[negative-argument]."
  (interactive "P")
  ;; Test if we add current region as search target.
  (when (and (region-active-p)
             (let ((search (car regexp-search-ring)))
               (or (not search)
                   (not (string-match-p
                         (format "^%s$" search)
                         (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (meow--push-search (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
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
          (let* ((m (match-data))
                 (marker-beg (car m))
                 (marker-end (cadr m))
                 (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                 (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
            (thread-first
              (meow--make-selection '(select . visit) beg end)
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
  "Read a regexp from minibuffer, then search and select it.

The input will be pushed into `regexp-search-ring'.  So
\\[meow-search] can be used for further searching with the same condition.

A list of occurred regexps will be provided for completion, the regexps will
be sanitized by default. To display them in raw format, set
`meow-visit-sanitize-completion' to nil.

To search backward, use \\[negative-argument]."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (text (meow--prompt-symbol-and-words
                (if arg "Visit backward: " "Visit: ")
                (point-min) (point-max)))
         (visit-point (meow--visit-point text reverse)))
    (if visit-point
        (let* ((m (match-data))
               (marker-beg (car m))
               (marker-end (cadr m))
               (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
               (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (thread-first
            (meow--make-selection '(select . visit) beg end)
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

(defun meow--thing-get-direction (cmd)
  (or
   (alist-get cmd meow-thing-selection-directions)
   'forward))

(defun meow-beginning-of-thing (thing)
  "Select to the beginning of THING."
  (interactive (list (meow-thing-prompt "Beginning of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'beginning)))
          (bounds (meow--parse-inner-of-thing-char thing)))
      (when bounds
        (thread-first
          (meow--make-selection '(select . transient)
                                (if back (point) (car bounds))
                                (if back (car bounds) (point)))
          (meow--select))))))

(defun meow-end-of-thing (thing)
  "Select to the end of THING."
  (interactive (list (meow-thing-prompt "End of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'end)))
          (bounds (meow--parse-inner-of-thing-char thing)))
      (when bounds
        (thread-first
          (meow--make-selection '(select . transient)
                                (if back (cdr bounds) (point))
                                (if back (point) (cdr bounds)))
          (meow--select))))))

(defun meow--select-range (back bounds)
  (when bounds
    (thread-first
      (meow--make-selection '(select . transient)
                            (if back (cdr bounds) (car bounds))
                            (if back (car bounds) (cdr bounds)))
      (meow--select))))

(defun meow-inner-of-thing (thing)
  "Select inner (excluding delimeters) of THING."
  (interactive (list (meow-thing-prompt "Inner of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'inner)))
          (bounds (meow--parse-inner-of-thing-char thing)))
      (meow--select-range back bounds))))

(defun meow-bounds-of-thing (thing)
  "Select bounds (including delimiters) of THING."
  (interactive (list (meow-thing-prompt "Bounds of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'bounds)))
          (bounds (meow--parse-bounds-of-thing-char thing)))
      (meow--select-range back bounds))))

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
   (t
    (meow--switch-state 'normal))))

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
           (sel-type (cons meow-expand-selection-type (cdr (meow--selection-type)))))
      (thread-first
        (meow--make-selection sel-type (mark)
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
   (let ((beg (caar (region-bounds)))
         (end (cdar (region-bounds)))
         (ov-list))
     (meow--wrap-collapse-undo
       ;; create overlays as marks at each line beginning.
       ;; apply kmacro at those positions.
       ;; these allow user executing kmacro those create newlines.
       (save-mark-and-excursion
         (goto-char beg)
         (while (< (point) end)
           (goto-char (line-beginning-position))
           (push (make-overlay (point) (point)) ov-list)
           (forward-line 1)))
       (cl-loop for ov in (reverse ov-list) do
                (goto-char (overlay-start ov))
                (thread-first
                  (meow--make-selection 'line (line-end-position) (line-beginning-position))
                  (meow--select))
                (call-last-kbd-macro)
                (delete-overlay ov))))))

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
        (thread-first
          (meow--make-selection '(select . visit)
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
            (progn
              (if back
                  (goto-char (min (point) (overlay-start ov)))
                (goto-char (max (point) (overlay-end ov))))
              (delete-overlay ov))))))))

(defun meow-start-kmacro ()
  "Start kmacro.

This command is a replacement for build-in `kmacro-start-macro'."
  (interactive)
  (cond
   ((or (meow-normal-mode-p) (meow-motion-mode-p))
    (call-interactively #'kmacro-start-macro))
   (t
    (message "Can only start recording in NORMAL or MOTION state."))))

(defun meow-start-kmacro-or-insert-counter ()
  "Start kmacro or insert counter.

This command is a replacement for build-in
 `kmacro-start-macro-or-insert-counter'."
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
   ((and meow--keypad-this-command defining-kbd-macro)
    (message "Can't end kmacro with KEYPAD command"))
   ((eq meow--beacon-defining-kbd-macro 'record)
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-end-and-apply-kmacro))
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
   (meow--keypad-this-command
    (message "Can't end kmacro with KEYPAD command"))
   ((or (meow-normal-mode-p)
        (meow-motion-mode-p))
    (call-interactively #'kmacro-end-or-call-macro))
   (t
    (message "Can only end or call kmacro in NORMAL or MOTION state."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAB SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meow--cancel-second-selection ()
  (delete-overlay mouse-secondary-overlay)
  (setq mouse-secondary-start (make-marker))
  (move-marker mouse-secondary-start (point)))

(defun meow-grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (secondary-selection-from-region)
    (meow--cancel-second-selection))
  (meow--cancel-selection))

(defun meow-pop-grab ()
  "Pop to secondary selection."
  (interactive)
  (cond
   ((meow--second-sel-buffer)
    (pop-to-buffer (meow--second-sel-buffer))
    (secondary-selection-to-region)
    (setq mouse-secondary-start (make-marker))
    (move-marker mouse-secondary-start (point))
    (meow--beacon-remove-overlays))
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
         (sel-str (meow--second-sel-get-string))
         (next-marker (make-marker)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (move-marker next-marker (point))
    (meow--second-sel-set-string (or region-str ""))
    (when (overlayp mouse-secondary-overlay)
       (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (meow--cancel-selection)))

(defun meow-sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (meow--with-selection-fallback
   (let* ((rbeg (region-beginning))
          (rend (region-end))
          (region-str (buffer-substring-no-properties rbeg rend))
          (next-marker (make-marker)))
     (move-marker next-marker (point))
     (meow--second-sel-set-string region-str)
     (when (overlayp mouse-secondary-overlay)
       (delete-overlay mouse-secondary-overlay))
     (setq mouse-secondary-start next-marker)
     (meow--cancel-selection))))

(defun meow-describe-key (key-list &optional buffer up-event)
  (interactive (list (help--read-key-sequence)))
  (if (= 1 (length key-list))
      (let* ((key (format-kbd-macro (cdar key-list)))
             (cmd (key-binding (cdar key-list))))
        (if-let ((dispatch (and (commandp cmd)
                                (get cmd 'meow-dispatch))))
            (describe-key (kbd dispatch) buffer up-event)
          (describe-key key-list buffer up-event)))
    ;; for mouse events
    (describe-key key-list buffer up-event)))

;; aliases
(defalias 'meow-backward-delete 'meow-backspace)
(defalias 'meow-c-d 'meow-C-d)
(defalias 'meow-c-k 'meow-C-k)
(defalias 'meow-delete 'meow-C-d)
(defalias 'meow-cancel 'meow-cancel-selection)

;; removed commands

(defmacro meow--remove-command (orig rep)
  `(defun ,orig ()
     (interactive)
     (message "Command removed, use `%s' instead." ,(symbol-name rep))))

(meow--remove-command meow-begin-of-buffer meow-beginning-of-thing)
(meow--remove-command meow-end-of-buffer meow-end-of-thing)
(meow--remove-command meow-pop meow-pop-selection)
(meow--remove-command meow-insert-at-begin meow-insert)
(meow--remove-command meow-append-at-end meow-append)
(meow--remove-command meow-head meow-left)
(meow--remove-command meow-tail meow-right)
(meow--remove-command meow-head-expand meow-left-expand)
(meow--remove-command meow-tail-expand meow-right-expand)
(meow--remove-command meow-block-expand meow-to-block)

(provide 'meow-command)
;;; meow-command.el ends here
