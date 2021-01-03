;;; meow-wgrep.el --- Make Meow play well with wgrep
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
;; Setup for wgrep.

;;; Code:

(require 'meow-var)

(declare-function meow-normal-mode "meow")
(declare-function meow-motion-mode "meow")

(defun meow--wgrep-to-normal (&rest ignore)
  "Switch to normal state, used in advice for wgrep.
Optional argument IGNORE ignored."
  (meow-normal-mode 1))

(defun meow--wgrep-to-motion (&rest ignore)
  "Switch to motion state, used in advice for wgrep.
Optional argument IGNORE ignored."
  (meow-motion-mode 1))

(defun meow--wgrep-setup (enable)
  "Setup wgrep.

We use advice here because wgrep doesn't call its hooks."
  (if enable
      (progn
        (advice-add 'wgrep-change-to-wgrep-mode :after #'meow--wgrep-to-normal)
        (advice-add 'wgrep-exit :after #'meow--wgrep-to-motion)
        (advice-add 'wgrep-finish-edit :after #'meow--wgrep-to-motion)
        (advice-add 'wgrep-save-all-buffers :after #'meow--wgrep-to-motion))
    (advice-remove 'wgrep-change-to-wgrep-mode #'meow--wgrep-to-normal)
    (advice-remove 'wgrep-exit #'meow--wgrep-to-motion)
    (advice-remove 'wgrep-finish-edit #'meow--wgrep-to-motion)
    (advice-remove 'wgrep-save-all-buffers #'meow--wgrep-to-motion)))

(provide 'meow-wgrep)
;;; meow-wgrep.el ends here
