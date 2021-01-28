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
          (overlay-put ov 'before-string (propertize "|" 'face 'meow-grab))
          (overlay-put ov 'face nil)
          (overlay-put ov 'evaporate (if init nil t)))
      (overlay-put ov 'face 'meow-grab)
      (overlay-put ov 'before-string nil)
      (overlay-put ov 'evaporate t)))
  (meow--update-indicator-for-grab))

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

(defun meow--grab-join-kill-ring (ring)
  (->> ring
       (-remove (lambda (s) (string-blank-p s)))
       (reverse)
       (-map-indexed (lambda (idx s)
                       (let* ((sel-type (get-text-property 0 'meow-selection-type s)))
                         (when sel-type
                           (remove-text-properties 0 (length s) 'meow-selection-type s)
                           (setq s (concat
                                    (unless (zerop idx)
                                      (cl-case (cdr sel-type)
                                        ((line) "\n")
                                        ((word symbol block) " ")))
                                    s)))
                         s)))
       (s-join "")))

(defmacro meow--with-kill-ring (&rest body)
  `(if (not (meow--has-grab-p))
       ,@body
     (let ((backup kill-ring)
           (kill-ring (list (meow--get-grab-string))))
       (when (region-active-p)
         (put-text-property (region-beginning)
                            (region-end)
                            'meow-selection-type
                            (meow--selection-type)))

       ,@body
       (unwind-protect
           (when (meow--has-grab-p)
             (save-mark-and-excursion
               (let ((p (overlay-start meow--grab))
                     (buf (overlay-buffer meow--grab)))
                 (with-current-buffer buf
                   (goto-char p)
                   (insert (meow--grab-join-kill-ring kill-ring))
                   (let ((end (overlay-end meow--grab)))
                     (when (> end (point))
                       (delete-region (point) end))
                     (meow--cancel-grab)
                     (meow--create-grab p (point)))))))
         (setq kill-ring backup)))))

(defmacro meow--with-pop-kill-ring-car (s &rest body)
  (declare (indent 1))
  `(let ((,s (car kill-ring)))
     ,@body
     (pop kill-ring)))

(defun meow--goto-grab ()
  (let ((buf (overlay-buffer meow--grab)))
    (with-current-buffer buf
      (goto-char (overlay-end meow--grab)))))

(defun meow--grab-indicator ()
  (or
   (and meow--grab
        (let ((buf (overlay-buffer meow--grab)))
          (or
           (when (equal (current-buffer) buf)
             (concat (car meow-grab-indicator) " "))
           (when (->> (window-list)
                      (-map #'window-buffer)
                      (member buf))
             (concat (cdr meow-grab-indicator) " ")))))
   ""))

(provide 'meow-grab)
;;; meow-grab.el ends here
