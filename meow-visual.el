;;; meow-visual.el --- Visual effect in Meow  -*- lexical-binding: t; -*-

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
(require 'pcase)

(require 'meow-var)
(require 'meow-util)

(declare-function hl-line-highlight "hl-line")

(defvar meow--expand-overlays nil
  "Overlays used to highlight expand hints in buffer.")

(defvar meow--match-overlays nil
  "Overlays used to highlight matches in buffer.")

(defvar meow--search-indicator-overlay nil
  "Overlays used to display search indicator in current line.")

(defvar-local meow--search-indicator-state nil
  "The state for search indicator.

Value is a list of (last-regexp last-pos idx cnt).")

(defvar meow--dont-remove-overlay nil
  "Indicate we should prevent removing overlay for once.")

(defvar meow--highlight-timer nil
  "Timer for highlight cleaner.")

(defun meow--remove-expand-highlights ()
  (mapc #'delete-overlay meow--expand-overlays)
  (setq meow--expand-overlays nil))

(defun meow--remove-match-highlights ()
  (mapc #'delete-overlay meow--match-overlays)
  (setq meow--match-overlays nil))

(defun meow--remove-search-highlight ()
  (when meow--search-indicator-overlay
    (delete-overlay meow--search-indicator-overlay)))

(defun meow--clean-search-indicator-state ()
  (setq meow--search-indicator-overlay nil
        meow--search-indicator-state nil))

(defun meow--remove-search-indicator ()
  (meow--remove-search-highlight)
  (meow--clean-search-indicator-state))

(defun meow--show-indicator (pos idx cnt)
  (goto-char pos)
  (goto-char (line-end-position))
  (if (= (point) (point-max))
      (let ((ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string (propertize (format " [%d/%d]" idx cnt) 'face 'meow-search-indicator))
        (setq meow--search-indicator-overlay ov))
    (let ((ov (make-overlay (point) (1+ (point)))))
      (overlay-put ov 'display (propertize (format " [%d/%d] \n" idx cnt) 'face 'meow-search-indicator))
      (setq meow--search-indicator-overlay ov))))

(defun meow--highlight-match ()
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (unless (cl-find-if (lambda (it)
                          (overlay-get it 'meow))
                        (overlays-at beg))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'meow-search-highlight)
        (overlay-put ov 'priority 0)
        (overlay-put ov 'meow t)
        (push ov meow--match-overlays)))))

(defun meow--highlight-regexp-in-buffer (regexp)
  "Highlight all regexp in this buffer."
  (when (and (meow-normal-mode-p)
             (region-active-p))
    (meow--remove-expand-highlights)
    (let* ((cnt 0)
           (idx 0)
           (pos (region-end))
           (hl-start (max (point-min) (- (point) 3000)))
           (hl-end (min (point-max) (+ (point) 3000))))
      (setq meow--expand-nav-function nil)
      (setq meow--visual-command this-command)
      (save-mark-and-excursion
        (meow--remove-search-indicator)
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward regexp (point-max) t)
            (cl-incf cnt)
            (when (<= (match-beginning 0) pos (match-end 0))
              (setq idx cnt))
            (when (<= hl-start (point) hl-end)
              (meow--highlight-match)))
          (meow--show-indicator pos idx cnt))))))

(defun meow--format-full-width-number (n)
  (alist-get n meow-full-width-number-position-chars))

(defun meow--highlight-num-positions-1 (nav-function faces bound)
  (save-mark-and-excursion
    (let ((pos (point))
          (i 1))
      (cl-loop for face in faces
               do
               (if-let ((r (funcall nav-function)))
                   (if (> r 0)
                       (save-mark-and-excursion
                         (goto-char r)
                         (if (or (> (point) (cdr bound))
                                 (< (point) (car bound))
                                 (= (point) pos))
                             (cl-return)
                           (setq pos (point))
                           (let ((ov (make-overlay (point) (1+ (point))))
                                 (before-full-width-char (and (char-after) (= 2 (char-width (char-after)))))
                                 (before-newline (equal 10 (char-after)))
                                 (before-tab (equal 9 (char-after)))
                                 (n (mod i 10)))
                             (overlay-put ov 'window (selected-window))
                             (cond
                              (before-full-width-char
                               (overlay-put ov 'display (propertize (format "%s" (meow--format-full-width-number n)) 'face face)))
                              (before-newline
                               (overlay-put ov 'display (concat (propertize (format "%s" n) 'face face) "\n")))
                              (before-tab
                               (overlay-put ov 'display (concat (propertize (format "%s" n) 'face face) "\t")))
                              (t
                               (overlay-put ov 'display (propertize (format "%s" n) 'face face))))
                             (push ov meow--expand-overlays)
                             (cl-incf i))))
                     (cl-return))
                 (cl-return))))))

(defun meow--highlight-num-positions (num)
  (setq meow--visual-command this-command)
  (meow--remove-expand-highlights)
  (meow--remove-match-highlights)
  (meow--remove-search-indicator)
  (let ((bound (cons (window-start) (window-end)))
        (faces (seq-take
                (if (meow--direction-backward-p)
                    (seq-concatenate
                     'list
                     (make-list 10 'meow-position-highlight-reverse-number-1)
                     (make-list 10 'meow-position-highlight-reverse-number-2)
                     (make-list 10 'meow-position-highlight-reverse-number-3))
                  (seq-concatenate
                   'list
                   (make-list 10 'meow-position-highlight-number-1)
                   (make-list 10 'meow-position-highlight-number-2)
                   (make-list 10 'meow-position-highlight-number-3)))
                num))
        (nav-function (if (meow--direction-backward-p)
                          (car meow--expand-nav-function)
                        (cdr meow--expand-nav-function))))
    (meow--highlight-num-positions-1 nav-function faces bound)
    (when meow--highlight-timer
      (cancel-timer meow--highlight-timer)
      (setq meow--highlight-timer nil))
    (setq meow--highlight-timer
          (run-at-time
           (time-add (current-time)
                     (seconds-to-time meow-expand-hint-remove-delay))
           nil
           #'meow--remove-expand-highlights))))

(defun meow--select-expandable-p ()
  (when (meow-normal-mode-p)
    (when-let ((sel (meow--selection-type)))
      (let ((type (cdr sel)))
        (member type '(word line block find till))))))

(defun meow--maybe-highlight-num-positions (&optional nav-functions)
  (when (and (meow-normal-mode-p)
             (meow--select-expandable-p))
    (setq meow--expand-nav-function (or nav-functions meow--expand-nav-function))
    (when (and (not (member major-mode meow-expand-exclude-mode-list))
               meow--expand-nav-function)
      (let ((num (alist-get (cdr (meow--selection-type)) meow-expand-hint-counts)))
        (meow--highlight-num-positions num)))))

(provide 'meow-visual)
;;; meow-visual.el ends here
