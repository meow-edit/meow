;;; meow-eldoc.el --- Make meow play well with eldoc
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
;; Set for eldoc.

;;; Code:

(defconst meow--eldoc-commands
  '(meow-head
    meow-tail
    meow-prev
    meow-next
    meow-next-line
    meow-prev-line
    meow-exp
    meow-word
    meow-backward-word
    meow-insert
    meow-insert-exit
    meow-append
    meow-open
    company-complete-common
    company-complete-common-or-cycle
    company-complete-selection)
  "A list meow commands trigger eldoc.")

(defun meow--eldoc-setup ()
  "Setup commands those trigger eldoc.
Basically, all navigation commands should trigger eldoc."
  (apply #'eldoc-add-command meow--eldoc-commands))

(provide 'meow-eldoc)
;;; meow-eldoc.el ends here
