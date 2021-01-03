;;; meow-shims.el --- Make Meow play well with other packages.
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
;; The file contains all the shim code we need to make meow
;; working with other packages.

;;; Code:

(require 'meow-var)

(declare-function meow-normal-mode "meow")
(declare-function meow-motion-mode "meow")
(declare-function meow-insert-exit "meow-command")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc

(defvar meow--setup-eldoc nil
  "If already setup eldoc.")

(defconst meow--eldoc-commands
  '(meow-head
    meow-tail
    meow-prev
    meow-next
    meow-next-word
    meow-mark-word
    meow-back-word
    meow-insert
    meow-append
    meow-open-below
    meow-open-above)
  "A list meow commands trigger eldoc.")

(defun meow--eldoc-setup (enable)
  "Setup commands those trigger eldoc.
Basically, all navigation commands should trigger eldoc."
  (setq meow--setup-eldoc enable)
  (if enable
      (apply #'eldoc-add-command meow--eldoc-commands)
    (apply #'eldoc-remove-command meow--eldoc-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(defvar meow--setup-company nil
  "If already setup company.")

(declare-function company--active-p "company")
(declare-function company-abort "company")

(defvar company-candidates)

(defun meow--company-maybe-abort-advice ()
  "Adviced for meow-insert-exit."
  (when company-candidates
    (company-abort)))

(defun meow--company-setup (enable)
  "Setup for company."
  (setq meow--setup-company enable)
  (if enable
      (advice-add 'meow-insert-exit :before #'meow--company-maybe-abort-advice)
    (advice-remove 'meow-insert-exit #'meow--company-maybe-abort-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep

(defvar meow--setup-wgrep nil
  "If already setup wgrep.")

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
  (setq meow--setup-wgrep enable)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet

(defvar meow--setup-yasnippet nil
  "If already setup yasnippet.")

(defun meow--yasnippet-setup (enable)
  "Setup for yasnippet."
  (setq meow--setup-yasnippet enable)
  (if enable
      (advice-add 'yas-abort-snippet :after #'meow-normal-mode)
    (advice-remove 'yas-abort-snippet #'meow-normal-mode)))

;;; meow-shims.el ends here
(provide 'meow-shims)
