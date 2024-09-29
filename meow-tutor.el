;;; meow-tutor.el --- Tutor for Meow  -*- lexical-binding: t; -*-

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
;; A tutorial for Meow.
;;
;; To start, with M-x meow-tutor

;;; Code:

(require 'meow-var)

(defconst meow--tutor-path
  (expand-file-name "tutor.txt" (file-name-directory load-file-name)))

(defconst meow--tutor-content
  (with-temp-buffer
    (insert-file-contents meow--tutor-path)
    (buffer-string)))

(defun meow-tutor ()
  "Open a buffer with meow tutor."
  (interactive)
  (let ((buf (get-buffer-create "*Meow Tutor*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format (substitute-command-keys meow--tutor-content)
                      (alist-get 'normal meow-replace-state-name-list)
                      (alist-get 'insert meow-replace-state-name-list)))
      (setq-local scroll-conservatively 1)
      (setq-local scroll-margin 3)
      (setq-local scroll-step 1)
      (goto-char (point-min))
      (display-line-numbers-mode))
    (switch-to-buffer buf)))

(provide 'meow-tutor)
;;; meow-tutor.el ends here
