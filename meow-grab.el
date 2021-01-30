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

The value is nil or a overlay.
We can only have one grab selection globally.")

(defun meow--update-indicator-for-grab ()
  (mapc (lambda (w)
          (with-current-buffer (window-buffer w)
            (meow--update-indicator)))
        (window-list)))

(defun meow--create-grab (beg end &optional init)
  "The selection will be created with the same bounds of current selection, or at current point."
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (point))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (ov (make-overlay beg end)))
    (setq meow--grab ov)
    (meow--grab-display init)))

(defun meow--grab-display (&optional init)
  (let ((ov meow--grab))
    (if (= (overlay-start ov)
           (overlay-end ov))
        (progn
          (overlay-put ov 'evaporate (if init nil t))
          (overlay-put ov 'before-string
                       (propertize (car meow-grab-delimiters) 'face 'meow-grab-delimiter))
          (overlay-put ov 'after-string
                       (propertize (cdr meow-grab-delimiters) 'face 'meow-grab-delimiter)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'face 'meow-grab)))
  (meow--update-indicator-for-grab))

(defun meow--own-grab-p ()
  "If grab selection is in current buffer."
  (and meow--grab
       (equal (current-buffer)
              (overlay-buffer meow--grab))))

(defun meow--has-grab-p ()
  "If grab selection is available.

The grab selection will only be available when it is visible in a window."
  (and meow--grab
       (let ((buf (overlay-buffer meow--grab)))
         (and (bufferp buf)
              (->> (window-list)
                   (-map #'window-buffer)
                   (member buf))))))

(defun meow--cancel-grab ()
  (when (overlayp meow--grab)
    (delete-overlay meow--grab))
  (setq meow--grab nil)
  (meow--update-indicator-for-grab))

(defun meow--get-grab-string ()
  (when (meow--has-grab-p)
    (let ((buf (overlay-buffer meow--grab))
          (beg (overlay-start meow--grab))
          (end (overlay-end meow--grab)))
      (with-current-buffer buf
        (buffer-substring-no-properties beg end)))))

(defun meow--grab-sync ()
  (when (meow--has-grab-p)
    (save-mark-and-excursion
      (let ((p (overlay-start meow--grab))
            (buf (overlay-buffer meow--grab)))
        (with-current-buffer buf
          (goto-char p)
          (insert (string-trim-right (current-kill 0) "\n"))
          (let ((end (overlay-end meow--grab)))
            (when (> end (point))
              (delete-region (point) end))
            (meow--cancel-grab)
            (meow--create-grab p (point))))))))

(defmacro meow--with-kill-ring (&rest body)
  `(if (not (meow--has-grab-p))
       ,@body
     (let ((inhibit-redisplay t)
           (cmd this-command))
       (unwind-protect
           (progn
             (kill-new (meow--get-grab-string))
             (when (region-active-p)
               (put-text-property (region-beginning)
                                  (region-end)
                                  'meow-selection-type
                                  (meow--selection-type)))
             ,@body)
         (meow--grab-sync)
         (when (member cmd meow-grab-auto-pop-commands)
           (meow--goto-grab)
           (meow--cancel-grab))))))

(defun meow--grab-undo ()
  (let* ((beg (overlay-start meow--grab))
         (end (overlay-end meow--grab))
         (at-min (= beg (point-min)))
         (at-max (= end (point-max))))
    (meow--cancel-grab)
    (meow--create-grab (if at-min (point-min) (1- beg))
                       (if at-max (point-max) (1+ end)))
    (meow--execute-kbd-macro meow--kbd-undo)
    (let ((beg (overlay-start meow--grab))
          (end (overlay-end meow--grab)))
      (meow--cancel-grab)
      (when beg (meow--create-grab (if at-min (point-min) (1+ beg))
                                   (if at-max (point-max) (1- end)))))))

(defun meow--goto-grab ()
  (let ((buf (overlay-buffer meow--grab)))
    (when (bufferp buf)
      (pop-to-buffer buf)
      (goto-char (overlay-end meow--grab)))))

(defun meow--grab-indicator ()
  (if (meow--own-grab-p)
      (concat meow-grab-indicator " ")
    ""))

(defun meow--grab-maybe-cancel ()
  (when meow--grab
    (unless (meow--has-grab-p)
      (meow--cancel-grab))))

(provide 'meow-grab)
;;; meow-grab.el ends here
