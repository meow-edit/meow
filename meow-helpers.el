;;; meow-helpers.el --- Meow Helpers for define keybinding
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

;;; Code:

(defun meow-leader-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key meow-leader-base-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

(defun meow-leader-define-mode-key (mode &rest args)
  (when-let ((keymap (meow--get-mode-leader-keymap mode t)))
    (mapcar (lambda (key-def)
              (define-key keymap
                (kbd (car key-def))
                (cdr key-def)))
            args)))

(defun meow-normal-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key meow-normal-state-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

(provide 'meow-helpers)
;;; meow-helpers.el ends here
