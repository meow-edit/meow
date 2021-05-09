;;; meow-face.el --- Faces for Meow  -*- lexical-binding: t; -*-

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
;; Faces for Meow.

;;; Code:

(defface meow-normal-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Normal state indicator."
  :group 'meow)

(defface meow-keypad-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Keypad indicator"
  :group 'meow)

(defface meow-insert-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Insert indicator"
  :group 'meow)

(defface meow-motion-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Motion indicator"
  :group 'meow)

(defface meow-normal-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Normal state cursor."
  :group 'meow)

(defface meow-insert-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Insert state cursor."
  :group 'meow)

(defface meow-motion-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Motion state cursor."
  :group 'meow)

(defface meow-keypad-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Keypad state cursor."
  :group 'meow)

(defface meow-unknown-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Unknown state cursor."
  :group 'meow)

(defface meow-search-highlight
  '((t (:inherit lazy-highlight)))
  "Search target highlight"
  :group 'meow)

(defface meow-position-highlight-number-1
  '((((class color) (background dark))
     (:foreground "grey90" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey20" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-number-2
  '((((class color) (background dark))
     (:foreground "grey70" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey40" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-number-3
  '((((class color) (background dark))
     (:foreground "grey60" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey60" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-1
  '((((class color) (background dark))
     (:foreground "grey90" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey20" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-2
  '((((class color) (background dark))
     (:foreground "grey70" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey40" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-3
  '((((class color) (background dark))
     (:foreground "grey60" :inverse-video t))
    (((class color) (background light))
     (:foreground "grey60" :inverse-video t)))
  "Num position highlight"
  :group 'meow)

(defface meow-search-indicator
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Face for search indicator."
  :group 'meow)

(defface meow-cheatsheet-command
  '((((class color) (background dark))
     (:height 0.7 :foreground "grey90"))
    (((class color) (background light))
     (:height 0.7 :foreground "grey10")))
  "Face for Meow cheatsheet command."
  :group 'meow)

(defface meow-cheatsheet-highlight
  '((((class color) (background dark))
     (:foreground "grey90"))
    (((class color) (background light))
     (:foreground "grey10")))
  "Face for Meow cheatsheet highlight text."
  :group 'meow)

(provide 'meow-face)
;;; meow-face.el ends here
