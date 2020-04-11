;;; meow-tut.el --- Tutorial of Meow
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
;; Provide a tutorial command `meow-tutorial'.

;;; Code:

(defconst meow--tutorial-buffer-name "*Meow Tutorial*")

(defconst meow--root (file-name-directory (or load-file-name buffer-file-name)))

(defun meow-tutorial ()
  "Open tutorial for Meow."
  (interactive)
  (with-current-buffer (get-buffer-create meow--tutorial-buffer-name)
    (erase-buffer)
    (insert-file-contents (expand-file-name "tutorial" meow--root))
    (goto-char (point-min))
    (text-mode))
  (switch-to-buffer meow--tutorial-buffer-name))

(provide 'meow-tut)
;;; meow-tut.el ends here
