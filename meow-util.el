;;; meow-util.el --- Utilities for Meow  -*- lexical-binding: t; -*-

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
;; Ultilities for Meow.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'seq)

(require 'meow-var)
(require 'meow-keymap)
(require 'meow-face)

;; Modes

(declare-function meow-insert-mode "meow-core")
(declare-function meow-motion-mode "meow-core")
(declare-function meow-normal-mode "meow-core")
(declare-function meow-keypad-mode "meow-core")
(declare-function meow-mode "meow-core")
(declare-function meow--keypad-format-keys "meow-keypad")
(declare-function meow--keypad-format-prefix "meow-keypad")
(declare-function meow-minibuffer-quit "meow-command")

(defun meow-insert-mode-p ()
  "Whether insert mode is enabled."
  (bound-and-true-p meow-insert-mode))

(defun meow-motion-mode-p ()
  "Whether motion mode is enabled."
  (bound-and-true-p meow-motion-mode))

(defun meow-normal-mode-p ()
  "Whether normal mode is enabled."
  (bound-and-true-p meow-normal-mode))

(defun meow-keypad-mode-p ()
  "Whether keypad mode is enabled."
  (bound-and-true-p meow-keypad-mode))

(defun meow--read-cursor-face-color (face)
  "Read cursor color from face."
  (let ((f (face-attribute face :inherit)))
    (if (equal 'unspecified f)
        (let ((color (face-attribute face :background)))
          (if (equal 'unspecified color)
              (face-attribute 'default :foreground)
            color))
      (meow--read-cursor-face-color f))))

(defun meow--set-cursor-color (face)
  "Set cursor color by face."
  (set-cursor-color (meow--read-cursor-face-color face)))

(defun meow--update-cursor ()
  "Update cursor type according to the current state.

For performance reasons, we save current cursor type to `meow--last-cursor-type' to avoid unnecessary updates."
  (cond
   ;; Don't alter the cursor-type when executing kmacro
   (executing-kbd-macro)
   ;; Don't alter the cursor-type if it's already hidden
   ((null cursor-type)
    (setq cursor-type meow-cursor-type-default)
    (meow--set-cursor-color 'meow-unknown-cursor))
   ((meow-insert-mode-p)
    (setq cursor-type meow-cursor-type-insert)
    (meow--set-cursor-color 'meow-insert-cursor))
   ((meow-normal-mode-p)
    (setq cursor-type meow-cursor-type-normal)
    (meow--set-cursor-color 'meow-normal-cursor))
   ((meow-motion-mode-p)
    (setq cursor-type meow-cursor-type-motion)
    (meow--set-cursor-color 'meow-motion-cursor))
   ((meow-keypad-mode-p)
    (setq cursor-type meow-cursor-type-keypad)
    (meow--set-cursor-color 'meow-keypad-cursor))
   (t
    (setq cursor-type meow-cursor-type-default)
    (meow--set-cursor-color 'meow-unknown-cursor))))

(defun meow--get-state-name (state)
  (alist-get state meow-replace-state-name-list))

(defun meow--render-indicator ()
  "Minimal indicator showing current mode."
  (when (bound-and-true-p meow-global-mode)
    (cond
     ((bound-and-true-p meow-keypad-mode)
      (propertize
       (format " %s " (meow--get-state-name 'keypad))
       'face 'meow-keypad-indicator))
     ((bound-and-true-p meow-normal-mode)
      (concat
       (propertize
        (format " %s " (meow--get-state-name 'normal))
        'face 'meow-normal-indicator)))
     ((bound-and-true-p meow-motion-mode)
      (propertize
       (format " %s " (meow--get-state-name 'motion))
       'face 'meow-motion-indicator))
     ((bound-and-true-p meow-insert-mode)
      (propertize
       (format " %s " (meow--get-state-name 'insert))
       'face 'meow-insert-indicator))
     (t ""))))

(defun meow--update-indicator ()
  (unless executing-kbd-macro
    (let ((indicator (meow--render-indicator)))
      (setq-local meow--indicator indicator))))

(defun meow--current-state ()
  (cond
   ((bound-and-true-p meow-insert-mode) 'insert)
   ((bound-and-true-p meow-normal-mode) 'normal)
   ((bound-and-true-p meow-motion-mode) 'motion)
   ((bound-and-true-p meow-keypad-mode) 'keypad)))

(defun meow--switch-state (state)
  "Switch to STATE."
  (unless (eq state (meow--current-state))
    (cl-case state
      ('insert
       (meow-insert-mode 1))
      ('normal
       (meow-normal-mode 1))
      ('motion
       (meow-motion-mode 1))
      ('keypad
       (meow-keypad-mode 1)))
    (run-hook-with-args 'meow-switch-state-hook state)
    (meow--update-indicator)
    (meow--update-cursor)))

(defun meow--exit-keypad-state ()
  "Exit keypad state."
  (meow-keypad-mode -1)
  (when meow--keypad-previous-state
    (meow--switch-state meow--keypad-previous-state)))

(defun meow--direction-forward ()
  "Make the selection towards forward."
  (when (and (region-active-p) (< (point) (mark)))
    (exchange-point-and-mark)))

(defun meow--direction-backward ()
  "Make the selection towards backward."
  (when (and (region-active-p) (> (point) (mark)))
    (exchange-point-and-mark)))

(defun meow--direction-backward-p ()
  "Return whether we have a backward selection."
  (and (region-active-p)
       (> (mark) (point))))

(defun meow--direction-forward-p ()
  "Return whether we have a forward selection."
  (and (region-active-p)
       (<= (mark) (point))))

(defun meow--selection-type ()
  "Return current selection type."
  (when (region-active-p)
    (car meow--selection)))

(defun meow--in-string-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 3 (syntax-ppss))))

(defun meow--in-comment-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 4 (syntax-ppss))))

(defun meow--prompt-symbol-and-words (prompt beg end)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((completions))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
        (let ((result (match-string-no-properties 0)))
          (when (>= (length result) meow-visit-collect-min-length)
            (if meow-visit-sanitize-completion
                (push (cons result (format "\\_<%s\\_>" (regexp-quote result))) completions)
              (push (format "\\_<%s\\_>" (regexp-quote result)) completions))))))
    (setq completions (delete-dups completions))
    (let ((selected (completing-read prompt completions nil nil)))
      (if meow-visit-sanitize-completion
          (or (cdr (assoc selected completions))
              (regexp-quote selected))
        selected))))

(defun meow--on-window-state-change (&rest args)
  "Update cursor style after switching window."
  (meow--update-cursor)
  (meow--update-indicator))

(defun meow--get-indent ()
  "Get indent of current line."
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun meow--empty-line-p ()
  "Whether current line is empty."
  (string-match-p "^ *$" (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

(defun meow--ordinal (n)
  (cl-case n
    ((1) "1st")
    ((2) "2nd")
    ((3) "3rd")
    (t (format "%dth" n))))

(defun meow--allow-modify-p ()
  (and (not buffer-read-only)
       (not meow--temp-normal)))

(defun meow--with-universal-argument-p (arg)
  (equal '(4) arg))

(defun meow--with-negative-argument-p (arg)
  (< (prefix-numeric-value arg) 0))

(defun meow--with-shift-p ()
  (member 'shift last-input-event))

(defun meow--bounds-with-type (type thing)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (cons type bounds)))

(defun meow--push-search (search)
  (unless (string-equal search (car regexp-search-ring))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

(defun meow--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun meow--toggle-relative-line-number ()
  (when display-line-numbers
    (if (bound-and-true-p meow-insert-mode)
        (setq display-line-numbers t)
      (setq display-line-numbers 'relative))))

(defun meow--render-char-thing-table ()
  (let* ((ww (frame-width))
         (w 16)
         (col (min 5 (/ ww w))))
    (->> (-map-indexed
          (-lambda (idx (c . th))
            (format "%s%s%s%s"
                    (propertize (s-pad-left 3 " " (char-to-string c)) 'face 'font-lock-constant-face)
                    (propertize " → " 'face 'font-lock-comment-face)
                    (propertize (s-pad-left 9 " " (symbol-name th)) 'face 'font-lock-function-name-face)
                    (if (= (1- col) (mod idx col))
                        "\n"
                      " ")))
          meow-char-thing-table)
         (s-join "")
         (s-trim-right))))

(defun meow--transpose-lists (lists)
  (when lists
    (let* ((n (-max (-map #'length lists)))
           (rst (apply #'list (-repeat n ()))))
      (-map (lambda (l)
              (-map-indexed
               (lambda (idx it)
                 (setq rst (-replace-at idx (cons it (nth idx rst)) rst)))
               l))
            lists)
      rst
      (-map #'seq-reverse rst))))

(defun meow--get-event-key (e)
  (if (and (integerp (event-basic-type e))
           (member 'shift (event-modifiers e)))
      (upcase (event-basic-type e))
    (event-basic-type e)))

(defun meow--ensure-visible ()
  (let ((overlays (overlays-at (1- (point))))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun meow--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'meow-minibuffer-quit)
  (setq-local meow-normal-mode nil)
  (when (or (member this-command meow-grab-fill-commands)
            (member meow--keypad-this-command meow-grab-fill-commands))
    (when-let ((s (meow--second-sel-get-string)))
      (insert s))))

(defun meow--parse-string-to-keypad-keys (str)
  (let ((strs (s-split " " str)))
    (->> strs
      (--map (cond
              ((s-prefix-p "C-M-" it)
               (cons 'both (substring it 4)))
              ((s-prefix-p "C-" it)
               (cons 'control (substring it 2)))
              ((s-prefix-p "M-" it)
               (cons 'meta (substring it 2)))
              (t
               (cons 'literal it))))
      (reverse))))

(defun meow--parse-input-event (e)
  (cond
   ((equal e 32)
    "SPC")
   ((characterp e)
    (string e))
   ((equal 'tab e)
    "TAB")
   ((equal 'return e)
    "RET")
   ((equal 'backspace e)
    "DEL")
   ((equal 'escape e)
    "ESC")
   ((symbolp e)
    (format "<%s>" e))
   (t nil)))

(defun meow--get-origin-command (key)
  (cdr (--find (equal (car it) key) meow--origin-commands)))

(defun meow--prepare-region-for-kill ()
  (when (and (equal '(expand . line) (meow--selection-type))
             (meow--direction-forward-p)
             (< (point) (point-max)))
    (forward-char 1)))

(defun meow--prepare-string-for-kill-append (s)
  (let ((curr (current-kill 0 nil)))
    (cl-case (cdr (meow--selection-type))
      ((line) (concat (unless (string-suffix-p "\n" curr) "\n")
                      (string-trim-right s "\n")))
      ((word block) (concat (unless (string-suffix-p " " curr) " ")
                            (string-trim s " " "\n")))
      (t s))))

(defun meow--event-key (e)
  (let ((c (event-basic-type e)))
    (if (member 'shift (event-modifiers e))
        (upcase c)
      c)))

(defun meow--parse-key-def (key-def)
  (if (stringp key-def)
      (lambda ()
        (interactive)
        (meow--execute-kbd-macro key-def))
    key-def))

(defun meow--second-sel-set-string (string)
  (cond
   ((meow--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (insert string)))))

(defun meow--second-sel-get-string ()
  (when (meow--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun meow--second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
       (overlay-buffer mouse-secondary-overlay)))

(defun meow--second-sel-bound ()
  (and (secondary-selection-exist-p)
       (cons (overlay-start mouse-secondary-overlay)
             (overlay-end mouse-secondary-overlay))))

(defmacro meow--wrap-collapse-undo (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             (prog1 ,(macroexp-progn body)
               (setq ,success t)))
         (if ,success
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defun meow--init-motion-p ()
  (let ((state-to-modes (--group-by (cdr it) meow-mode-state-list)))
    (or (apply #'derived-mode-p
               (-map #'car (alist-get 'motion state-to-modes)))
        (not (apply #'derived-mode-p
                    (-map #'car (alist-get 'normal state-to-modes)))))))

(defun meow--highlight-pre-command ()
  (unless (member this-command '(meow-search))
    (meow--remove-match-highlights))
  (meow--remove-expand-highlights)
  (meow--remove-search-highlight))

(provide 'meow-util)
;;; meow-util.el ends here
