;;; meow-yas.el --- Make Meow play well with yasnippet
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
;; Setup for yasnippet.
;; - Back to normal state when we abort the snippet.

;;; Code:

(require 'meow-var)

(declare-function meow-normal-mode "meow")

(defun meow--yas-setup (enable)
  "Setup for yasnippet."
  (if enable
      (advice-add 'yas-abort-snippet :after #'meow-normal-mode)
    (advice-remove 'yas-abort-snippet #'meow-normal-mode)))

(provide 'meow-yas)
;;; meow-yas.el ends here
