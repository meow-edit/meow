;;; meow-cheatsheet.el --- Cheatsheet for Meow  -*- lexical-binding: t; -*-

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
;; Cheatsheet for Meow.

;;; Code:

(require 'meow-var)
(require 'meow-util)
(require 'meow-cheatsheet-layout)

(defconst meow--cheatsheet-note
  (format "
NOTE:
%s means this command will expand current region.
" (propertize "ex" 'face 'meow-cheatsheet-highlight)))

(defun meow--render-cheatsheet-thing-table ()
  (concat
   (format
    "%s, %s, %s and %s require a %s as input:\n"
    (propertize "←thing→ (inner)" 'face 'meow-cheatsheet-highlight)
    (propertize "[thing] (bounds)" 'face 'meow-cheatsheet-highlight)
    (propertize "←thing (begin)" 'face 'meow-cheatsheet-highlight)
    (propertize "thing→ (end)" 'face 'meow-cheatsheet-highlight)
    (propertize "THING" 'face 'meow-cheatsheet-highlight))
   (meow--cheatsheet-render-char-thing-table 'meow-cheatsheet-highlight)))

(defvar meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi
  "Physical keyboard layout used to display cheatsheet.

Currently `meow-cheatsheet-physical-layout-ansi' is supported.")

(defvar meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
  "Keyboard layout used to display cheatsheet.

Currently `meow-cheatsheet-layout-qwerty', `meow-cheatsheet-layout-dvorak',
`meow-cheatsheet-layout-dvp' and `meow-cheatsheet-layout-colemak' is supported.")

(defun meow--short-command-name (cmd)
  (or
   (when (symbolp cmd)
     (when-let ((s
                 (or (alist-get cmd meow-command-to-short-name-list)
                     (cl-case cmd
                       (undefined "")
                       (t (thread-last
                            (symbol-name cmd)
                            (replace-regexp-in-string "meow-" "")))))))
       (if (<= (length s) 9)
           (format "% 9s" s)
         (meow--truncate-string 9 s meow-cheatsheet-ellipsis))))
   "         "))

(defun meow--cheatsheet-replace-keysyms ()
  (dolist (it meow-cheatsheet-layout)
    (let* ((keysym (car it))
           (lower (cadr it))
           (upper (caddr it))
           (tgt (concat "  " (symbol-name keysym) " "))
           (lower-cmd (key-binding (read-kbd-macro lower)))
           (upper-cmd (key-binding (read-kbd-macro upper))))
      (goto-char (point-min))
      (when (search-forward tgt nil t)
        (let ((x (- (point) (line-beginning-position))))
          (backward-delete-char 9)
          (insert (concat "       " upper " "))
          (forward-line 1)
          (forward-char x)
          (backward-delete-char 9)
          (insert (propertize (meow--short-command-name upper-cmd) 'face 'meow-cheatsheet-highlight))
          (forward-line 2)
          (forward-char x)
          (backward-delete-char 9)
          (insert (concat "       " lower " "))
          (forward-line 1)
          (forward-char x)
          (backward-delete-char 9)
          (insert (propertize (meow--short-command-name lower-cmd) 'face 'meow-cheatsheet-highlight)))))))

(defun meow--cheatsheet-render-char-thing-table (&optional key-face)
  (let* ((ww (frame-width))
         (w 16)
         (col (min 5 (/ ww w))))
    (thread-last
      (seq-map-indexed
       (lambda (it idx)
         (let ((c (car it))
               (th (cdr it)))
           (format "% 9s ->% 3s%s"
                   (symbol-name th)
                   (propertize (char-to-string c) 'face (or key-face 'font-lock-keyword-face))
                   (if (= (1- col) (mod idx col))
                       "\n"
                     " "))))
       meow-char-thing-table)
      (string-join)
      (string-trim-right))))

(defun meow-cheatsheet ()
  (interactive)
  (cond
   ((not meow-cheatsheet-physical-layout)
    (message "`meow-cheatsheet-physical-layout' is not specified"))
   ((not meow-cheatsheet-layout)
    (message "`meow-cheatsheet-layout' is not specified"))
   (t
    (let ((buf (get-buffer-create (format "*Meow Cheatsheet*"))))
    (with-current-buffer buf
      (text-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply #'insert (make-list 63 " "))
      (insert "Meow Cheatsheet\n")
      (insert meow-cheatsheet-physical-layout)
      (meow--cheatsheet-replace-keysyms)
      (goto-char (point-max))
      (insert meow--cheatsheet-note)
      (insert (meow--render-cheatsheet-thing-table))
      (put-text-property (point-min) (point-max) 'display '(height 0.8))
      (setq buffer-read-only t))
    (switch-to-buffer buf)))))

(provide 'meow-cheatsheet)
;;; meow-cheatsheet.el ends here
