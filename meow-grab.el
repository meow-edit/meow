;;; meow-grab.el --- Secondary selection in Meow -*- lexical-binding: t -*-

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

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'meow-face)
(require 'meow-util)

(defvar meow--grab nil
  "The grab selection.

The value can be nil or an overlay.")

(defun meow--has-grab-p ()
  "Whether grab selection is available.

The grab selection will only be available when it is visible in a window."
  (when-let ((buf (-some-> meow--grab (overlay-buffer))))
    (->> (window-list)
         (-map #'window-buffer)
         (member buf))))

(defun meow--create-grab-overlay (point &optional mark is-init)
  (when (overlayp meow--grab) (delete-overlay meow--grab))
  (let ((ov (make-overlay point (or mark point))))
    (if mark
        (progn
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'face 'meow-grab))
      (overlay-put ov 'evaporate (if is-init nil t))
      (overlay-put ov 'before-string (propertize (car meow-grab-delimiters) 'face 'meow-grab-delimiter))
      (overlay-put ov 'after-string (propertize (cdr meow-grab-delimiters) 'face 'meow-grab-delimiter)))
    (setq meow--grab ov)))

(defun meow--get-grab-string ()
  (when (meow--has-grab-p)
    (let ((beg (overlay-start meow--grab))
          (end (overlay-end meow--grab)))
      (with-current-buffer (overlay-buffer meow--grab)
        (buffer-substring beg end)))))

(defun meow--grab-start ()
  "Start Grab with current point or region.

Current region text will be pushed to kill-ring.
An overlay will be used as grab indicator.
We can only have one grab selection global"
  (if (region-active-p)
      (meow--create-grab-overlay (point) (mark) t)
    (meow--create-grab-overlay (point) nil t))
  (kill-new (meow--get-grab-string)))

(defun meow--grab-cancel ()
  "Cancel Grab, pop kill-ring."
  (delete-overlay meow--grab)
  (setq meow--grab nil))

(defun meow--grab-pop ()
  "Similar to `meow-grab-cancel', but jump to grab selection."
  (let ((buf (overlay-buffer meow--grab)))
    (when (bufferp buf)
      (pop-to-buffer buf)
      (goto-char (overlay-end meow--grab))
      (meow--grab-cancel))))

(defun meow--grab-maybe-sync ()
  (when (meow--has-grab-p)
    (let* ((buf (overlay-buffer meow--grab))
          (beg (overlay-start meow--grab))
          (end (overlay-end meow--grab))
          (curr-kill (string-trim (current-kill 0) "[\r\n]*")))
      (save-mark-and-excursion
        (with-current-buffer buf
          (goto-char beg)
          (delete-region beg end)
          (insert curr-kill)
          (meow--create-grab-overlay (point) beg))))))

(defmacro meow--with-grab-sync (&rest body)
  (let ((this-cmd (gensym)))
    ;; Save this-command, call-interactively will overwrite this variable.
    `(let ((,this-cmd this-command))
       ,@body
       (meow--grab-maybe-sync)
       (when (member ,this-cmd meow-grab-auto-pop-commands)
         (when (meow--has-grab-p)
           (meow--grab-pop))))))

(defun meow--grab-maybe-cancel ()
  (when meow--grab
    (unless (meow--has-grab-p)
      (meow--grab-cancel))))

(provide 'meow-grab)
;;; meow-grab.el ends here
