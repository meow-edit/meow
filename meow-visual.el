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

(require 'dash)
(require 'subr-x)

(defun meow--remove-highlight-regexp ()
  (when (not (member this-command '(meow-visit meow-search meow-reverse)))
    (mapc (lambda (it) (delete-overlay it)) meow--highlight-regexp-overlays)
    (setq meow--highlight-regexp-overlays nil))
  (remove-hook 'post-command-hook #'meow--remove-text-properties t))

(defvar meow--highlight-regexp-overlays nil
  "Overlays used to highlight regexps.")

(defun meow--highlight-regexp-in-buffer (regexp)
  "Highlight all REGEXP in this buffer."
  (save-mark-and-excursion
    (mapc (lambda (it) (delete-overlay it)) meow--highlight-regexp-overlays)
    (setq meow--highlight-regexp-overlays nil)
    (goto-char (window-start))
    (let ((case-fold-search nil))
	  (while (re-search-forward regexp (window-end) t)
	    (let ((ov (make-overlay (match-beginning 0)
							    (match-end 0))))
		  (overlay-put ov 'face 'meow-search-highlight)
		  (push ov meow--highlight-regexp-overlays)))))
  (add-hook 'post-command-hook #'meow--remove-highlight-regexp t t))

(provide 'meow-visual)
;;; meow-visual.el ends here
