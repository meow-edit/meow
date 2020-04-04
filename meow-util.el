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

(defun meow--update-cursor ()
  (cond
   (meow-insert-mode
    (setq cursor-type '(bar . 2)))
   (meow-normal-mode
    (setq cursor-type 'box))
   (meow-motion-mode
    (setq cursor-type 'box))
   (meow-keypad-mode
    (setq cursor-type 'hollow))
   (t
    (setq cursor-type 'box))))

(defun meow--switch-state (state)
  (cl-case state
    ('insert
     (meow-insert-mode 1))
    ('normal
     (meow-normal-mode 1))
    ('motion
     (meow-motion-mode 1))
    ('keypad
     (meow-keypad-mode 1))))

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

(defun meow--in-string-p ()
  "Return if we are in string."
  (nth 3 (syntax-ppss)))

(defun meow--in-comment-p ()
  "Return if we are in string."
  (nth 4 (syntax-ppss)))

(defun meow--prompt-symbol-and-words (beg end)
  (let ((list))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+" end t)
        (let ((result (match-string-no-properties 0)))
          (push result list)))
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\)+" end t)
        (let ((result (match-string-no-properties 0)))
          (push result list))))
    (setq list (delete-dups list))
    (completing-read "Select: " list nil nil)))

(defun meow--get-mode-leader-keymap (mode &optional ensure)
  "Return the leader keymap for mode.
If ensure is t, create new if not found."
  (if-let ((keymap (plist-get meow--leader-mode-keymaps mode)))
      keymap
    (if ensure
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap meow-leader-base-keymap)
        (setq meow--leader-mode-keymaps (plist-put meow--leader-mode-keymaps mode keymap))
        keymap)
      meow-leader-base-keymap)))

(defun meow--post-command-function ()
  (meow--auto-switch-mode)
  (meow--update-cursor))

(defun meow--auto-switch-mode ()
  (let ((use-normal (apply #'derived-mode-p meow-normal-state-mode-list)))
    (unless (apply #'derived-mode-p meow-auto-switch-exclude-mode-list)
      (cond
       ((and (or meow-insert-mode meow-normal-mode)
             (not use-normal))
        (meow--switch-state 'motion)
        (message "Meow: Auto switch to MOTION state."))
       ((and meow-motion-mode use-normal)
        (meow--switch-state 'normal)
        (message "Meow: Auto switch to NORMAL state."))))))

(defun meow--get-indent ()
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun meow--empty-line-p ()
  (string-match-p "^ *$" (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

(provide 'meow-util)
;;; meow-util.el ends here
