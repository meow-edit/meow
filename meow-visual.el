;;; meow-visual.el --- Visual effect in Meow
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

(defun meow--remove-highlights ()
  (mapc (lambda (it) (delete-overlay it)) meow--highlight-overlays)
  (setq meow--highlight-overlays nil))

(defvar meow--highlight-overlays nil
  "Overlays used to highlight in buffer.")

(defun meow--highlight-regexp-in-buffer (regexp)
  "Highlight all REGEXP in this buffer."
  (save-mark-and-excursion
    (setq meow--visual-command this-command)
    (meow--remove-highlights)
    (goto-char (window-start))
    (let ((case-fold-search nil))
      (while (re-search-forward regexp (window-end) t)
        (let ((ov (make-overlay (match-beginning 0)
                                (match-end 0))))
          (overlay-put ov 'face 'meow-search-highlight)
          (push ov meow--highlight-overlays))))))

(defun meow--format-number (n)
  (alist-get n meow-number-position-chars))

(defun meow--remove-highlight-overlays ()
  (unless (or (equal this-command meow--visual-command)
              (member this-command
                      '(meow-expand
                        meow-expand-0
                        meow-expand-1
                        meow-expand-2
                        meow-expand-3
                        meow-expand-4
                        meow-expand-5
                        meow-expand-6
                        meow-expand-7
                        meow-expand-8
                        meow-expand-9)))
    (meow--remove-highlights)
    (setq meow--visual-command nil
          meow--expand-nav-function nil)))

(defun meow--highlight-num-positions-1 (nav-function faces bound)
  (save-mark-and-excursion
    (cl-loop for face in faces
             do
             (cl-loop for i from 1 to 10 do
                      (funcall nav-function)
                      (if (or (> (point) (cdr bound))
                              (< (point) (car bound))
                              (= (point) pos))
                          (cl-return)
                        (setq pos (point))
                        (let ((ov (make-overlay (point) (1+ (point))))
                              (before-newline (equal 10 (char-after)))
                              (before-tab (equal 9 (char-after)))
                              (n (if (= i 10) 0 i)))
                          (cond
                           (before-newline
                            (overlay-put ov 'display (propertize (format "%s\n" (meow--format-number n)) 'face face)))
                           (before-tab
                            (overlay-put ov 'display (propertize (format "%s\t" (meow--format-number n)) 'face face)))
                           (t
                            (overlay-put ov 'display (propertize (format "%s" (meow--format-number n)) 'face face))))
                          (push ov meow--highlight-overlays)))))))

(defun meow--highlight-num-positions (&optional nav-functions)
  (when-let ((nav-functions (or nav-functions meow--expand-nav-function)))
    (setq meow--expand-nav-function nav-functions)
    (setq meow--visual-command this-command)
    (meow--remove-highlights)
    (-let ((pos (point))
           (bound (cons (window-start) (window-end)))
           (faces1 '(meow-position-highlight-number-1
                     meow-position-highlight-number-2
                     meow-position-highlight-number-3))
           (faces2 '(meow-position-highlight-reverse-number-1
                     meow-position-highlight-reverse-number-2
                     meow-position-highlight-reverse-number-3)))
      (save-mark-and-excursion
        (meow--direction-forward)
        (meow--highlight-num-positions-1 (cdr nav-functions)
                                         faces1
                                         bound))
      (save-mark-and-excursion
        (meow--direction-backward)
        (meow--highlight-num-positions-1 (car nav-functions)
                                         faces2
                                         bound)))))

(provide 'meow-visual)
;;; meow-visual.el ends here
