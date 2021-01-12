;;; meow-util.el --- Utilities for Meow
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

(defun meow-insert-mode-p ()
  "If insert mode is enabled."
  (bound-and-true-p meow-insert-mode))

(defun meow-motion-mode-p ()
  "If motion mode is enabled."
  (bound-and-true-p meow-motion-mode))

(defun meow-normal-mode-p ()
  "If normal mode is enabled."
  (bound-and-true-p meow-normal-mode))

(defun meow-keypad-mode-p ()
  "If keypad mode is enabled."
  (bound-and-true-p meow-keypad-mode))

(defun meow--set-cursor-color (face)
  (let ((color (face-attribute face :background)))
    (if (equal 'unspecified color)
        (set-cursor-color (face-attribute 'default :foreground))
      (when (stringp color)
        (set-cursor-color color)))))

(defun meow--update-cursor ()
  "Update cursor type according to current state."
  (cond
   ;; Don't alter cursor-type if it's already hidden
   ((null cursor-type))
   ((minibufferp)
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
  "Minimal indicator show current mode."
  (when (bound-and-true-p meow-global-mode)
    (cond
     ((bound-and-true-p meow-keypad-mode)
      (propertize
       (format " %s " (meow--get-state-name 'keypad))
       'face 'meow-keypad-indicator))
     ((bound-and-true-p meow-normal-mode)
      (propertize
       (format " %s " (meow--get-state-name 'normal))
       'face 'meow-normal-indicator))
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
  (let ((indicator (meow--render-indicator)))
    (setq-local meow--indicator indicator)))

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
    (meow--update-cursor)
    (meow--update-indicator)))

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
  "Return if we have a backward selection."
  (and (region-active-p)
       (> (mark) (point))))

(defun meow--selection-type ()
  "Return current selection type."
  (when (region-active-p)
    (car meow--selection)))

(defun meow--in-string-p (&optional pos)
  "Return if POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 3 (syntax-ppss))))

(defun meow--in-comment-p (&optional pos)
  "Return if POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 4 (syntax-ppss))))

(defun meow--prompt-symbol-and-words (prompt beg end)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((list))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+" end t)
        (let ((result (match-string-no-properties 0)))
          (push (format "\\_<%s\\_>" (regexp-quote result)) list))))
    (setq list (delete-dups list))
    (completing-read prompt list nil nil)))

(defun meow--init-state (&rest ignore)
  "Enable meow unless we are in minibuffer."
  (unless (or meow-mode (minibufferp))
    (meow-mode 1)))

(defun meow--on-window-state-change (arg)
  "Initialize or change meow state in this buffer."
  (meow--update-cursor))

(defun meow--auto-switch-mode ()
  "Switch to correct state."
  (let ((use-normal (or (apply #'derived-mode-p meow-normal-state-mode-list)
						(equal major-mode 'fundamental-mode))))
    (unless (apply #'derived-mode-p meow-auto-switch-exclude-mode-list)
      (cond
	   ((minibufferp)
		(meow--update-cursor))
       ((and (or (meow-insert-mode-p) (meow-normal-mode-p))
             (not use-normal)
			 (not (minibufferp)))
        (meow--switch-state 'motion))
       ((and (meow-motion-mode-p) use-normal)
		(meow--switch-state 'normal))
       ((not (bound-and-true-p meow-mode))
		(if (minibufferp)
			(meow--switch-state 'insert)
          (meow--switch-state 'normal)))))))

(defun meow--get-indent ()
  "Get indent of current line."
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun meow--empty-line-p ()
  "If current line is empty."
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
  (unless (string-equal search (car meow--recent-searches))
    (push search meow--recent-searches)
    (when (> (length meow--recent-searches) 100)
      (setq meow--recent-searches (-take 100 meow--recent-searches)))))

(defun meow--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun meow--add-newline-to-recent-kill-ring ()
  (let ((yank-text (pop kill-ring)))
    (if (string-suffix-p "\n" yank-text)
        (push yank-text kill-ring)
      (push (format "%s\n" yank-text) kill-ring))))

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

(defun meow--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'meow-minibuffer-quit)
  (setq-local meow-normal-mode nil))

(provide 'meow-util)
;;; meow-util.el ends here
